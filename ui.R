library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# Define UI for dashboard
dashboardPage(
  # Dashboard header
  dashboardHeader(title = "Steam Game Database Analytics"),
  
  # Dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Game Analysis", tabName = "tab1", icon = icon("gamepad")),
      menuItem("Developer Insights", tabName = "tab2", icon = icon("users")),
      menuItem("Category Breakdown", tabName = "tab3", icon = icon("chart-pie"))
    )
  ),
  
  # Dashboard body
  dashboardBody(
    tabItems(
      # TAB 1 - Game Analysis
      tabItem(tabName = "tab1",
              fluidRow(
                box(
                  title = "Platform Filter", status = "primary", solidHeader = TRUE,
                  width = 3, height = 200,
                  selectInput("platform", "Select Platform:",
                              choices = c("All", "Windows", "Mac", "Linux"),
                              selected = "All")
                ),
                box(
                  title = "Year Range", status = "primary", solidHeader = TRUE,
                  width = 3, height = 200,
                  sliderInput("yearRange", "Release Year:",
                              min = 2007, max = 2023, value = c(2010, 2020),
                              step = 1, sep = "")
                ),
                box(
                  title = "Top Dev Reflection", status = "success", solidHeader = TRUE,
                  width = 6, height = 200,
                  h4("Game Development Insights"),
                  p("Top 20 developers analysis"),
                  p("Top 50 publishers overview")
                )
              ),
              fluidRow(
                box(
                  title = "Game Release Over Time", status = "info", solidHeader = TRUE,
                  width = 8, height = 400,
                  plotOutput("gameReleaseChart")
                ),
                box(
                  title = "Distribution Genre", status = "warning", solidHeader = TRUE,
                  width = 4, height = 400,
                  plotOutput("genreDistribution")
                )
              ),
              fluidRow(
                box(
                  title = "Games Browser Table - Age Distribution", status = "primary", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("gamesTable")
                )
              )
      ),
      
      # TAB 2 - Developer Insights  
      tabItem(tabName = "tab2",
              fluidRow(
                box(
                  title = "Genre Selection", status = "primary", solidHeader = TRUE,
                  width = 4, height = 200,
                  selectInput("genreFilter", "Select Genre:",
                              choices = c("All", "Action", "Adventure", "Strategy", "RPG", "Simulation"),
                              selected = "All")
                ),
                box(
                  title = "Age Selection", status = "success", solidHeader = TRUE,
                  width = 4, height = 200,
                  selectInput("ageFilter", "Age Rating:",
                              choices = c("All", "Everyone", "Teen", "Mature"),
                              selected = "All")
                ),
                box(
                  title = "Min Games (Last 2 Years)", status = "warning", solidHeader = TRUE,
                  width = 4, height = 200,
                  numericInput("minGames", "Minimum Games:",
                               value = 2, min = 1, max = 10)
                )
              ),
              fluidRow(
                box(
                  title = "Developer Distribution", status = "info", solidHeader = TRUE,
                  width = 8, height = 400,
                  plotOutput("developerChart")
                ),
                box(
                  title = "Publisher Distribution", status = "primary", solidHeader = TRUE,
                  width = 4, height = 400,
                  plotOutput("publisherChart")
                )
              ),
              fluidRow(
                box(
                  title = "Game Category Distribution", status = "success", solidHeader = TRUE,
                  width = 12, height = 300,
                  plotOutput("categoryPieChart")
                )
              )
      ),
      
      # TAB 3 - Category Breakdown
      tabItem(tabName = "tab3",
              fluidRow(
                box(
                  title = "Games Filter", status = "primary", solidHeader = TRUE,
                  width = 3, height = 150,
                  numericInput("totalGames", "Total Games:",
                               value = 1234, min = 100, max = 5000)
                ),
                box(
                  title = "Unique Games", status = "success", solidHeader = TRUE,
                  width = 3, height = 150,
                  numericInput("uniqueGames", "Unique Games:",
                               value = 10, min = 1, max = 100)
                ),
                box(
                  title = "Release Year", status = "warning", solidHeader = TRUE,
                  width = 3, height = 150,
                  numericInput("releaseYear", "Focus Year:",
                               value = 2020, min = 2000, max = 2023)
                ),
                box(
                  title = "Year Over Pub", status = "info", solidHeader = TRUE,
                  width = 3, height = 150,
                  numericInput("yearOverPub", "Publishers:",
                               value = 3, min = 1, max = 20)
                )
              ),
              fluidRow(
                box(
                  title = "Category Analysis by Year", status = "primary", solidHeader = TRUE,
                  width = 12, height = 450,
                  plotOutput("categoryAnalysis")
                )
              ),
              fluidRow(
                box(
                  title = "Filtering Options", status = "success", solidHeader = TRUE,
                  width = 12, height = 200,
                  fluidRow(
                    column(3, h5("Music")),
                    column(3, h5("Category")),
                    column(3, h5("Genre")),
                    column(3, h5("Age"))
                  ),
                  fluidRow(
                    column(3, checkboxInput("musicFilter", "Include Music Games", TRUE)),
                    column(3, selectInput("catSelect", "Category:", choices = c("All", "Action", "Strategy"))),
                    column(3, selectInput("genreSelect", "Genre:", choices = c("All", "RPG", "Shooter"))),
                    column(3, selectInput("ageSelect", "Age:", choices = c("All", "E", "T", "M")))
                  )
                )
              )
      )
    )
  )
)