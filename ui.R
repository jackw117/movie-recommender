## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

movie.genres = strsplit(movies$Genres, split = "|", fixed = TRUE, useBytes = TRUE)
unique.genre.combos = unique(movie.genres)
unique.genre = sort(unique(unlist(movie.genres)))

shinyUI(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Movie Recommender"),
        
        dashboardSidebar(disable = TRUE),
        
        dashboardBody(includeCSS("css/movies.css"),
                      fluidRow(
                          box(width = 12, title = "Select your favorite movie genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              selectInput("fav", "Genre:", choices = unique.genre)
                          )
                      ),
                      fluidRow(
                          useShinyjs(),
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
        )
    )
) 