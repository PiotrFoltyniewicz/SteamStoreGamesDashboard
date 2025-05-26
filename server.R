library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(dplyr)

# Define server logic
function(input, output, session) {
  
  # Sample data creation (replace with your actual data)
  games_data <- reactive({
    set.seed(42)
    data.frame(
      game_name = paste("Game", 1:1000),
      year = sample(2007:2023, 1000, replace = TRUE),
      genre = sample(c("Action", "Adventure", "Strategy", "RPG", "Simulation", "Sports"), 1000, replace = TRUE),
      platform = sample(c("Windows", "Mac", "Linux"), 1000, replace = TRUE, prob = c(0.7, 0.2, 0.1)),
      developer = sample(paste("Developer", LETTERS[1:50]), 1000, replace = TRUE),
      publisher = sample(paste("Publisher", 1:30), 1000, replace = TRUE),
      age_rating = sample(c("Everyone", "Teen", "Mature"), 1000, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
      rating = round(runif(1000, 1, 10), 1),
      price = round(runif(1000, 5, 70), 2)
    )
  })
  
  # TAB 1 Outputs
  output$gameReleaseChart <- renderPlot({
    data <- games_data()
    
    # Filter by platform if not "All"
    if(input$platform != "All") {
      data <- data[data$platform == input$platform, ]
    }
    
    # Filter by year range
    data <- data[data$year >= input$yearRange[1] & data$year <= input$yearRange[2], ]
    
    # Aggregate by year
    yearly_counts <- data %>%
      group_by(year) %>%
      summarise(count = n(), .groups = 'drop')
    
    ggplot(yearly_counts, aes(x = year, y = count)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "darkblue", size = 2) +
      theme_minimal() +
      labs(title = "Game Releases Over Time",
           x = "Year", 
           y = "Number of Games Released") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$genreDistribution <- renderPlot({
    data <- games_data()
    
    genre_counts <- data %>%
      group_by(genre) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count))
    
    ggplot(genre_counts, aes(x = "", y = count, fill = genre)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Genre Distribution") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_brewer(palette = "Set3")
  })
  
  output$gamesTable <- DT::renderDataTable({
    data <- games_data()
    
    # Select relevant columns for display
    display_data <- data %>%
      select(game_name, year, genre, developer, publisher, age_rating, rating) %>%
      arrange(desc(year))
    
    DT::datatable(display_data, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  colnames = c("Game", "Year", "Genre", "Developer", "Publisher", "Age", "Rating"))
  })
  
  # TAB 2 Outputs
  output$developerChart <- renderPlot({
    data <- games_data()
    
    # Apply filters
    if(input$genreFilter != "All") {
      data <- data[data$genre == input$genreFilter, ]
    }
    if(input$ageFilter != "All") {
      data <- data[data$age_rating == input$ageFilter, ]
    }
    
    # Get top developers
    dev_counts <- data %>%
      group_by(developer) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(15)
    
    ggplot(dev_counts, aes(x = reorder(developer, count), y = count)) +
      geom_col(fill = "lightblue", color = "navy") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Developers by Game Count",
           x = "Developer", 
           y = "Number of Games") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$publisherChart <- renderPlot({
    data <- games_data()
    
    pub_counts <- data %>%
      group_by(publisher) %>%
      summarise(count = n(), .groups = 'drop') %>%
      arrange(desc(count)) %>%
      head(10)
    
    ggplot(pub_counts, aes(x = reorder(publisher, count), y = count)) +
      geom_col(fill = "lightcoral", color = "darkred") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top Publishers",
           x = "Publisher", 
           y = "Games Published") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$categoryPieChart <- renderPlot({
    data <- games_data()
    
    # Create categories based on age rating
    category_counts <- data %>%
      group_by(age_rating) %>%
      summarise(count = n(), .groups = 'drop')
    
    ggplot(category_counts, aes(x = "", y = count, fill = age_rating)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(title = "Game Category Distribution by Age Rating") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = c("Everyone" = "lightgreen", 
                                   "Teen" = "orange", 
                                   "Mature" = "red"))
  })
  
  # TAB 3 Outputs
  output$categoryAnalysis <- renderPlot({
    data <- games_data()
    
    # Multi-panel analysis
    year_genre <- data %>%
      group_by(year, genre) %>%
      summarise(count = n(), .groups = 'drop')
    
    ggplot(year_genre, aes(x = year, y = count, fill = genre)) +
      geom_area(position = "stack", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Category Analysis Over Time",
           x = "Year", 
           y = "Number of Games",
           fill = "Genre") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_brewer(palette = "Set2")
  })
}