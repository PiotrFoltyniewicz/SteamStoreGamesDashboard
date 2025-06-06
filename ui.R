library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

dashboardPage(
  skin = "blue",
  
  dashboardHeader( title = "Steam Analytics"
  ),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview & Trends", tabName = "overview", icon = icon("chart-line")),
      menuItem("Genre Analytics", tabName = "genres", icon = icon("gamepad")),
      menuItem("Games Explorer", tabName = "games", icon = icon("search")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    tags$div(
      style = "position: absolute; bottom: 10px; width: 100%; text-align: center;",
      tags$img(src = "./PP_znak_konturowy_WHITE.png", height = "150px", style = "margin-bottom: 10px;")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Main layout styling */
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        
        /* Sidebar styling */
        .main-sidebar {
          background: #2c3e50 !important;
        }
        
        .sidebar-menu > li > a {
          color: white !important;
          border-bottom: 1px solid #34495e;
          transition: background 0.3s;
        }
        
        .sidebar-menu > li > a:hover,
        .sidebar-menu > li.active > a {
          background: #3498db !important;
          color: white !important;
        }
        
        /* Header styling */
        .main-header .navbar {
          background: #3498db !important;
          border-bottom: 1px solid #2980b9;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        .main-header .navbar-brand {
          color: white !important;
          width: 100% !important;
          text-align: center !important;
        }
        
        .main-header .navbar-brand:hover {
          color: #ecf0f1 !important;
        }
        
        /* Show hamburger menu icon and position it properly */
        .navbar-toggle {
          display: block !important;
          float: left !important;
          margin-left: 15px !important;
          margin-right: 15px !important;
          background-color: transparent !important;
          border: 1px solid rgba(255,255,255,0.3) !important;
        }
        
        .navbar-toggle:hover,
        .navbar-toggle:focus {
          background-color: rgba(255,255,255,0.1) !important;
        }
        
        .navbar-toggle .icon-bar {
          background-color: white !important;
        }
        
        /* Adjust title position to accommodate hamburger menu */
        .main-header .navbar-brand {
          margin-left: 50px !important;
        }
        
        /* Controls panel styling */
        .controls-panel {
          background: white;
          padding: 20px;
          margin-bottom: 20px;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          border: none;
        }
        
        .controls-row {
          display: flex;
          justify-content: space-around;
          flex-wrap: wrap;
          align-items: end;
          gap: 20px;
        }
        
        .control-group {
          flex: 1;
          min-width: 200px;
          max-width: 300px;
        }
        
        .control-group label {
          display: block;
          margin-bottom: 5px;
          font-weight: bold;
          color: #2c3e50;
          font-size: 16px;
        }
        
        .control-group .form-control {
          border: 1px solid #ddd;
          border-radius: 4px;
          font-size: 14px;
        }
        
        /* Custom slider styling for better year display */
        .irs-grid-text {
          font-size: 10px !important;
          font-weight: bold !important;
          color: #2c3e50 !important;
          white-space: nowrap !important;
        }
        
        .irs-grid-pol {
          background-color: #3498db !important;
        }
        
        .irs-bar {
          background: linear-gradient(to right, #3498db, #2980b9) !important;
        }
        
        .irs-from,
        .irs-to,
        .irs-single {
          background-color: #3498db !important;
          font-weight: bold !important;
        }
        
        .irs-handle {
          border: 2px solid #3498db !important;
          background-color: white !important;
        }
        
        /* Value boxes styling */
        .value-box {
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          border-left: 4px solid #3498db;
        }
        
        .value-box .value-box-value {
          font-size: 28px !important;
          color: #2c3e50;
        }
        
        .value-box .value-box-text {
          color: #7f8c8d;
          font-size: 14px;
        }
        
        .value-box .value-box-icon {
          padding-top: 15px;
        }
        
        /* Box styling */
        .box {
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          border-top: none;
        }
        
        .box-header {
          border-bottom: 2px solid #3498db;
          padding: 15px 20px;
        }
        
        .box-header .box-title {
          font-size: 16px;
          font-weight: bold;
          color: #2c3e50;
          margin: 0;
        }
        
        .box-body {
          padding: 20px;
        }
        
        /* Chart placeholder styling */
        .chart-placeholder {
          height: 300px;
          background: #f8f9fa;
          border: 2px dashed #dee2e6;
          display: flex;
          align-items: center;
          justify-content: center;
          color: #6c757d;
          font-style: italic;
          border-radius: 4px;
          text-align: center;
        }
        
        /* Info text styling */
        .mockup-text {
          font-size: 12px;
          color: #95a5a6;
          text-align: center;
          margin-top: 10px;
          font-style: italic;
        }
        
        /* Responsive design */
        @media (max-width: 768px) {
          .controls-row {
            flex-direction: column;
          }
          
          .control-group {
            min-width: 100%;
          }
        }
        
        /* Table styling */
        .dataTables_wrapper {
          padding: 0;
        }
        
        /* Ensure proper spacing */
        .content {
          padding: 20px;
        }
        
        /* About section specific styling */
        .about-section {
          background: white;
          padding: 30px;
          border-radius: 8px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        
        .about-section h3 {
          color: #2c3e50;
          border-bottom: 2px solid #3498db;
          padding-bottom: 10px;
          margin-bottom: 15px;
        }
        
        .about-section ul {
          padding-left: 20px;
        }
        
        .about-section li {
          margin-bottom: 8px;
          line-height: 1.6;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
              div(class = "controls-panel",
                  div(class = "controls-row",
                      div(class = "control-group",
                          tags$label("Year Range:"),
                          sliderInput("yearRange", NULL,
                                      min = 1997, max = 2019, value = c(2000, 2019),
                                      step = 1, sep = "",
                                      ticks = TRUE),
                          tags$script(HTML("
                            $(document).ready(function() {
                              setTimeout(function() {
                                $('#yearRange').parent().find('.irs-grid-text').each(function(index) {
                                  if (index % 2 === 1) {
                                    $(this).hide();
                                  }
                                });
                              }, 100);
                            });
                          "))
                      ),
                      div(class = "control-group",
                          tags$label("Platform:"),
                          selectInput("platform", NULL,
                                      choices = list(
                                        "All Platforms" = "all",
                                        "Windows" = "windows", 
                                        "Linux" = "linux",
                                        "Mac" = "mac"
                                      ),
                                      selected = "all")
                      )
                  )
              ),
              
              fluidRow(
                valueBoxOutput("totalGames", width = 3),
                valueBoxOutput("peakYear", width = 3),
                valueBoxOutput("uniqueDevs", width = 3),
                valueBoxOutput("publishers", width = 3)
              ),
              
              fluidRow(
                box(
                  title = HTML("📈 Annual Game Releases Timeline"), 
                  status = "primary", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("timelineChart", height = "300px"),
                  div(class = "mockup-text", "Showing trends in game releases over time")
                ),
                box(
                  title = HTML("🎮 Top Game Genres Distribution"), 
                  status = "primary", solidHeader = TRUE,
                  width = 6, height = 450,
                  plotlyOutput("genreChart", height = "300px"),
                  div(class = "mockup-text", "Most popular game categories on Steam")
                )
              )
      ),
      
      tabItem(tabName = "genres",
              div(class = "controls-panel",
                  div(class = "controls-row",
                      div(class = "control-group",
                          tags$label("Select Genres:"),
                          selectInput("genreFilter", NULL,
                                      choices = list(
                                        "Indie" = "indie",
                                        "Action" = "action",
                                        "Casual" = "casual",
                                        "Adventure" = "adventure",
                                        "Strategy" = "strategy",
                                        "Simulation" = "simulation",
                                        "RPG" = "rpg",
                                        "Sports" = "sports",
                                        "Racing" = "racing",
                                        "Violent" = "violent",
                                        "Gore" = "gore",
                                        "Nudity" = "nudity"
                                      ),
                                      multiple = TRUE,
                                      selected = c("action", "adventure", "indie", "strategy"))
                      )
                  )
              ),
              
              fluidRow(
                box(
                  title = HTML("💰 Price Distribution by Genre"), 
                  status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("priceChart", height = "300px"),
                  div(class = "mockup-text", "Average pricing across different game genres")
                ),
                box(
                  title = HTML("⏱️ Median Playtime by Genre"), 
                  status = "success", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("playtimeChart", height = "300px"),
                  div(class = "mockup-text", "How long players spend in different game types")
                )
              ),
              
              fluidRow(
                box(
                  title = HTML("⭐ User Ratings by Genre"), 
                  status = "warning", solidHeader = TRUE,
                  width = 12, height = 550,
                  plotlyOutput("ratingsChart", height = "400px"),
                  div(class = "mockup-text", "Community ratings and review scores by genre")
                )
              )
      ),
      
      tabItem(
        tabName = "games",
        fluidRow(
          column(12,
                 div(
                   style = "text-align: center; margin: 20px 0; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
                   h3(textOutput("selectedGameTitle"), style = "color: #2c3e50; margin: 0;")
                 )
          )
        ),
        
        fluidRow(
          valueBoxOutput("gameReleaseDate", width = 4),
          valueBoxOutput("gamePositiveRating", width = 4),
          valueBoxOutput("gameMedianPlaytime", width = 4)
        ),
        
        fluidRow(
          box(
            title = "🔍 Search & Filter Games", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            collapsible = TRUE,
            div(
              style = "margin-bottom: 15px;",
              fluidRow(
                column(4,
                       div(class = "control-group",
                           tags$label("Select Genres:"),
                           selectInput("explorerGenreFilter", NULL,
                                       choices = list(
                                         "Indie" = "indie",
                                         "Action" = "action",
                                         "Casual" = "casual",
                                         "Adventure" = "adventure",
                                         "Strategy" = "strategy",
                                         "Simulation" = "simulation",
                                         "RPG" = "rpg",
                                         "Early Access" = "early_access",
                                         "Free to Play" = "free_to_play",
                                         "Sports" = "sports",
                                         "Racing" = "racing",
                                         "Violent" = "violent",
                                         "Massively Multiplayer" = "massively_multiplayer",
                                         "Gore" = "gore",
                                         "Nudity" = "nudity",
                                         "Sexual Content" = "sexual_content"
                                       ),
                                       multiple = TRUE,
                                       selected = c("action", "adventure"))
                       )
                ),
                column(4,
                       div(class = "control-group",
                           tags$label("Price Range ($):"),
                           sliderInput("priceRange",
                                       NULL,
                                       min = 0,
                                       max = 150,
                                       value = c(0, 100),
                                       step = 1,
                                       pre = "$")
                       )
                ),
                column(4,
                       div(class = "control-group",
                           tags$label("Search Games:"),
                           textInput("searchText", 
                                     NULL,
                                     placeholder = "Search by name, developer, publisher...")
                       )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            offset = 1,
            style = "width:82%",
            box( 
              title = "🎮 Steam Games Database",
              status = "info",
              solidHeader = TRUE,
              width = 12,
              DT::dataTableOutput("gamesDT")
            )
          )
          
        ),
      ),
      tabItem(tabName = "about",
              fluidRow(
                column(12,
                       div(class = "about-section",
                           h2("Steam Games Dashboard", style = "text-align: center; color: #2c3e50; margin-bottom: 30px;"),
                           
                           h3("About"),
                           p("We made this dashboard to analyze Steam games data - it has info on over 27,000 games. It's basically our class project where we looked at different stats about games on Steam."),
                           
                           h3("What's in here:"),
                           tags$ul(
                             tags$li("Charts showing when games came out over time"),
                             tags$li("Analysis of game genres and which ones are popular"),
                             tags$li("Search function to find specific games"),
                             tags$li("User ratings and review data"),
                             tags$li("Game pricing information")
                           ),
                           
                           h3("Made by"),
                           p("Piotr Foltyniewicz 160295"),
                           p("Jakub Adamski 160291")
                       )
                )
              )
      )
    )
  )
)