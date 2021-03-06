# I use the project here as a template for my app: https://github.com/pspachtholz/BookRecommender
# as well as the code here: https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html

## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Movie Recommender"),
        
        dashboardSidebar(
            sidebarMenu(
                # Setting id makes input$tabs give the tabName of currently-selected tab
                id = "tabs",
                menuItem("System I", tabName = "s1", icon = icon("dashboard")),
                menuItem("System II", icon = icon("th"), tabName = "s2")
            )
        ),
        
        dashboardBody(includeCSS("css/movies.css"),
          tabItems(
              tabItem("s1",
                  fluidRow(
                      box(width = 12, title = "Select your favorite movie genre", status = "info", solidHeader = TRUE,
                          uiOutput('genres')
                      )
                  ),
                  fluidRow(
                      box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                      )
                  )
              ),
              tabItem("s2",
                  fluidRow(
                      box(width = 12, title = "Rate as many movies as you can", status = "info", solidHeader = TRUE, 
                          div(class = "rateitems",
                              uiOutput('ratings')
                          )
                      )
                  ),
                  fluidRow(
                      useShinyjs(),
                      box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                              actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("recommendations")
                      )
                  )
              )
          )
        )
    )
) 