library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)

function(input, output, session) {
  
  games_data <- reactive({
    tryCatch({
      steam_data <- read_csv("./steam.csv", 
                             col_types = cols(
                               appid = col_double(),
                               name = col_character(),
                               release_date = col_character(),
                               english = col_double(),
                               developer = col_character(),
                               publisher = col_character(),
                               platforms = col_character(),
                               required_age = col_double(),
                               categories = col_character(),
                               genres = col_character(),
                               steamspy_tags = col_character(),
                               achievements = col_double(),
                               positive_ratings = col_double(),
                               negative_ratings = col_double(),
                               average_playtime = col_double(),
                               median_playtime = col_double(),
                               owners = col_character(),
                               price = col_double()
                             ))
      
      steam_data <- steam_data %>%
        filter(!is.na(name), !is.na(release_date)) %>%
        mutate(
          release_date = as.Date(release_date, format = "%Y-%m-%d"),
          year = year(release_date),
          
          developer = ifelse(is.na(developer) | developer == "", "Unknown", developer),
          publisher = ifelse(is.na(publisher) | publisher == "", "Unknown", publisher),
          
          has_windows = str_detect(tolower(platforms), "windows"),
          has_mac = str_detect(tolower(platforms), "mac"),
          has_linux = str_detect(tolower(platforms), "linux"),
          
          platform = case_when(
            has_windows & (has_mac | has_linux) ~ "Multi-platform",
            has_windows ~ "Windows",
            has_mac & !has_windows ~ "Mac",
            has_linux & !has_windows ~ "Linux",
            TRUE ~ "Other"
          ),
          
          genres_clean = ifelse(is.na(genres) | genres == "", "Unknown", genres),
          primary_genre = ifelse(
            genres_clean == "Unknown",
            "Unknown", 
            str_extract(genres_clean, "^[^;]+")
          ),
          price = ifelse(is.na(price), 0, price),
        )
      return(steam_data)
      
    })
  })
  
  filtered_data <- reactive({
    data <- games_data()
    
    if (!is.null(input$yearRange)) {
      data <- data %>% filter(year >= input$yearRange[1], year <= input$yearRange[2])
    }
    
    if (!is.null(input$platform) && input$platform != "all") {
      platform_map <- list(
        "windows" = "Windows", 
        "linux" = "Linux",
        "mac" = "Mac"
      )
      if (input$platform %in% names(platform_map)) {
        data <- data %>% filter(platform == platform_map[[input$platform]])
      }
    }
    return(data)
  })
  
  output$totalGames <- renderValueBox({
    total_games <- nrow(filtered_data())
    valueBox(
      value = format(total_games, big.mark = ","),
      subtitle = "Total Games",
      icon = icon("gamepad"),
      color = "blue"
    )
  })
  
  output$peakYear <- renderValueBox({
    data <- filtered_data()
    peak_year <- data %>%
      count(year) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(year)
    
    if (length(peak_year) == 0) peak_year <- "N/A"
    
    valueBox(
      value = peak_year,
      subtitle = "Peak Release Year", 
      icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$uniqueDevs <- renderValueBox({
    unique_devs <- filtered_data() %>%
      distinct(developer) %>%
      nrow()
    
    valueBox(
      value = format(unique_devs, big.mark = ","),
      subtitle = "Unique Developers",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  output$publishers <- renderValueBox({
    unique_pubs <- filtered_data() %>%
      distinct(publisher) %>%
      nrow()
    
    valueBox(
      value = format(unique_pubs, big.mark = ","), 
      subtitle = "Publishers",
      icon = icon("building"),
      color = "red"
    )
  })
  
  output$timelineChart <- renderPlotly({
    data <- filtered_data()
    
    yearly_counts <- data %>%
      group_by(year) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(year)
    
    p <- ggplot(yearly_counts, aes(x = year, y = count)) +
      geom_line(color = "#3498db", size = 1.2) +
      geom_point(color = "#2980b9", size = 2) +
      theme_minimal() +
      labs(x = "Year", y = "Games Released") +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        title = list(text = "", font = list(size = 14)),
        hovermode = "x unified"
      )
  })
  
  output$genreChart <- renderPlotly({
    data <- filtered_data()
    
    genre_counts <- data %>%
      count(primary_genre, sort = TRUE) %>%
      filter(primary_genre != "Unknown") %>%
      head(8) %>%
      mutate(percentage = round(n/sum(n)*100, 1))
    
    colors <- c("#3498db", "#e74c3c", "#f39c12", "#2ecc71", "#9b59b6", "#1abc9c", "#e67e22", "#34495e")
    
    plot_ly(genre_counts, 
            labels = ~primary_genre, 
            values = ~n, 
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colors[1:nrow(genre_counts)], 
                          line = list(color = '#FFFFFF', width = 2))) %>%
      layout(
        title = list(text = "", font = list(size = 14)),
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1, y = 0.5)
      )
  })
  
  tab2_filtered_data <- reactive({
    data <- games_data()
    genres = c("indie", "action", "casual", "adventure", "strategy", "simulation", 
               "rpg", "sports", "racing", "violent", "gore","nudity")
    
    data <- data[tolower(data$primary_genre) %in% genres, ]
    if (!is.null(input$genreFilter) && length(input$genreFilter) > 0) {
      data <- data[tolower(data$primary_genre) %in% input$genreFilter, ]
    }
    return(data)
  })
  
  output$priceChart <- renderPlotly({
    data <- tab2_filtered_data() %>%
      filter(price <= 75)
    
    p <- ggplot(data, aes(x = reorder(primary_genre, price, FUN = median), 
                          y = price)) +
      geom_violin(fill = "#34a1eb", 
                  alpha = 0.7, 
                  scale = "width",
                  trim = TRUE,
                  show.legend = FALSE) +
      scale_fill_viridis_d(option = "plasma") +
      scale_y_continuous(
        limits = c(0, min(75, max(data$price))),
        breaks = seq(0, 75, by = 5)  # This must be outside of limits
      ) +
      labs(x = "Genre", 
           y = "Price ($)") +
      theme_minimal() +
      coord_flip()
    
    ggplotly(p, tooltip = "none")
  })
  
  output$playtimeChart <- renderPlotly({
    data <- tab2_filtered_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    density_data <- data %>%
      filter(!is.na(median_playtime), median_playtime >= 1, median_playtime < 1500, primary_genre != "Unknown")
    
    p <- ggplot(density_data, aes(x = median_playtime, color = primary_genre, fill = primary_genre)) +
      geom_density(alpha = 0.3) +
      labs(
        x = "Median Playtime (minutes)",
        y = "Relative probability of playtime",
        title = "Distribution of Median Playtime by Primary Genre",
        color = "Genre",
        fill = "Genre") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5)
      )
      theme_minimal()
    
    ggplotly(p, tooltip = "none")
  })
  
  output$ratingsChart <- renderPlotly({
    data <- tab2_filtered_data()
    
    if (nrow(data) == 0) {
      return(data.frame(Message = "No data available"))
    }
    
    ratings_summary <- data %>%
      filter(
        !is.na(positive_ratings), 
        !is.na(negative_ratings), 
        positive_ratings + negative_ratings >= 10, 
        primary_genre != "Unknown"
      ) %>%
      group_by(primary_genre) %>%
      summarise(
        total_positive = sum(positive_ratings, na.rm = TRUE),
        total_negative = sum(negative_ratings, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(
        total = total_positive + total_negative,
        pos_share = total_positive / total,
        neg_share = total_negative / total,
        primary_genre = factor(primary_genre, levels = rev(unique(primary_genre)))
      )
    
    plot_data <- ratings_summary %>%
      select(primary_genre, pos_share, neg_share) %>%
      pivot_longer(cols = c(pos_share, neg_share),
                   names_to = "Sentiment", values_to = "Percent") %>%
      mutate(
        Percent = ifelse(Sentiment == "neg_share", -Percent, Percent),
        Sentiment = ifelse(Sentiment == "pos_share", "Positive", "Negative")
      )
    
    p <- ggplot(plot_data, aes(x = primary_genre, y = Percent, fill = Sentiment)) +
      geom_col(width = 0.6) +
      geom_vline(xintercept = 0, color = "black") +
      coord_flip() +
      theme_minimal() +
      scale_fill_manual(values = c("Positive" = "#3CBB75FF", "Negative" = "#FF3B30")) +
      labs(
        x = NULL,
        y = "Share of Ratings (Normalized)",
        fill = "Sentiment"
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(axis.text.y = element_text(size = 12))
    
    ggplotly(p, tooltip = "none")
  })
  
  table_filtered_data <- reactive({
    data <- games_data()
    
    if (!is.null(input$explorerGenreFilter) && length(input$explorerGenreFilter) > 0) {
      genre_mapping <- list(
        "indie" = "Indie",
        "action" = "Action", 
        "casual" = "Casual",
        "adventure" = "Adventure",
        "strategy" = "Strategy",
        "simulation" = "Simulation",
        "rpg" = "RPG",
        "early_access" = "Early Access",
        "free_to_play" = "Free to Play",
        "sports" = "Sports",
        "racing" = "Racing",
        "violent" = "Violent",
        "massively_multiplayer" = "Massively Multiplayer",
        "gore" = "Gore",
        "nudity" = "Nudity",
        "sexual_content" = "Sexual Content"
      )
      
      selected_genres <- unlist(genre_mapping[input$explorerGenreFilter])
      
      data <- data %>%
        filter(tolower(primary_genre) %in% tolower(selected_genres))
    }
    
    if (!is.null(input$minPrice)) {
      data <- data %>% filter(price >= input$minPrice)
    }
    
    if (!is.null(input$maxPrice)) {
      data <- data %>% filter(price <= input$maxPrice)
    }
    
    return(data)
  })
  
  selected_game_data <- reactive({
    selected_row <- input$gamesDT_rows_selected
    data <- table_filtered_data()
    
    if (length(selected_row) > 0) {
      selected_game <- data[selected_row, ]
      return(selected_game)
    } else {
      return(data.frame(
        name = "No game selected",
        release_date = as.Date(NA),
        positive_ratings = 0,
        negative_ratings = 0,
        median_playtime = 0
      ))
    }
  })
  
  output$selectedGameTitle <- renderText({
    game_data <- selected_game_data()
    if (is.na(game_data$name) || game_data$name == "No game selected") {
      "Select a game from the table below to view details"
    } else {
      paste("Game Details:", game_data$name)
    }
  })
  
  output$gameReleaseDate <- renderValueBox({
    game_data <- selected_game_data()
    
    release_date <- if (is.na(game_data$release_date)) {
      "N/A"
    } else {
      format(game_data$release_date, "%B %d, %Y")
    }
    
    valueBox(
      value = release_date,
      subtitle = "Release Date",
      icon = icon("calendar-alt"),
      color = "blue"
    )
  })
  
  output$gamePositiveRating <- renderValueBox({
    game_data <- selected_game_data()
    
    positive <- ifelse(is.na(game_data$positive_ratings), 0, game_data$positive_ratings)
    negative <- ifelse(is.na(game_data$negative_ratings), 0, game_data$negative_ratings)
    total <- positive + negative
    
    percentage <- if (total > 0) {
      round((positive / total) * 100, 1)
    } else {
      0
    }
    
    valueBox(
      value = paste0(percentage, "%"),
      subtitle = "Positive Ratings",
      icon = icon("thumbs-up"),
      color = if (percentage >= 80) "green" else if (percentage >= 60) "yellow" else "red"
    )
  })
  
  output$gameMedianPlaytime <- renderValueBox({
    game_data <- selected_game_data()
    
    playtime <- ifelse(is.na(game_data$median_playtime), 0, game_data$median_playtime)
    
    if (playtime >= 60) {
      hours <- round(playtime / 60, 1)
      display_value <- paste0(hours, " hrs")
    } else {
      display_value <- paste0(playtime, " min")
    }
    
    valueBox(
      value = display_value,
      subtitle = "Median Playtime",
      icon = icon("clock"),
      color = "purple"
    )
  })
  
  output$gamesDT <- DT::renderDataTable({
    data <- table_filtered_data()
    
    # Apply price range filter
    if(!is.null(input$priceRange)) {
      data <- data %>%
        filter(price >= input$priceRange[1] & price <= input$priceRange[2])
    }
    
    # Apply search filter
    if(!is.null(input$searchText) && input$searchText != "") {
      search_term <- tolower(input$searchText)
      data <- data %>%
        filter(
          grepl(search_term, tolower(name), fixed = TRUE) |
            grepl(search_term, tolower(developer), fixed = TRUE) |
            grepl(search_term, tolower(publisher), fixed = TRUE) |
            grepl(search_term, tolower(primary_genre), fixed = TRUE) |
            grepl(search_term, tolower(platform), fixed = TRUE)
        )
    }
    
    display_data <- data %>%
      select(
        Name = name,
        Developer = developer,
        Publisher = publisher,
        Genre = primary_genre,
        Platform = platform,
        `Price ($)` = price,
        Achievements = achievements
      ) %>%
      mutate(
        `Price ($)` = round(`Price ($)`, 2),
        Achievements = ifelse(is.na(Achievements), 0, Achievements)
      )
    
    DT::datatable(
      display_data,
      selection = "single",
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 25, 50, 100),
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'lrtip',  # Removed 'f' to hide default search box
        columnDefs = list(
          list(className = 'dt-center', targets = 3:6),
          list(width = '200px', targets = 0),
          list(width = '150px', targets = c(1, 2)),
          list(width = '100px', targets = c(3, 4, 5, 6))
        )
      ),
      rownames = FALSE,
      filter = "none",  # Changed from "top" to "none" to remove column filters
      escape = FALSE,
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center; color: #333; font-size: 16px; font-weight: bold;',
        'Steam Games Database - Click a row to view details'
      )
    ) %>%
      DT::formatCurrency(c("Price ($)"), currency = "$", digits = 2)
  })
  
}