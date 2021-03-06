#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# I use the project here as a template for my app: https://github.com/pspachtholz/BookRecommender
# as well as the code here: https://liangfgithub.github.io/Rcode_W13_Movie_RS.nb.html

library(shiny)
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(data.table)
library(reshape2)

myurl = "https://liangfgithub.github.io/MovieData/"

get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
}

# ratings
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

# read in data
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

tmp = ratings %>% 
    group_by(MovieID) %>% 
    summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
    inner_join(movies, by = 'MovieID')

top.100 = tmp %>% arrange(desc(ratings_per_movie))
top.100 = top.100[1:100,]

m = top.100[[100,'ratings_per_movie']]
C = sum(tmp$ave_ratings) / nrow(tmp)
tmp$ave_ratings = ((tmp$ave_ratings * tmp$ratings_per_movie) + (C * m)) / (tmp$ratings_per_movie + m)
top.weighted = tmp %>% arrange(desc(ave_ratings))

top.rated = tmp %>% arrange(desc(ratings_per_movie))

# users
users = read.csv(paste0(myurl, 'users.dat?raw=true'),
                 sep = ':', header = FALSE)
users = users[, -c(2,4,6,8)] # skip columns
colnames(users) = c('UserID', 'Gender', 'Age', 'Occupation', 'Zip-code')

genre_list <- c("Action", "Adventure", "Animation", "Children", "Comedy", "Crime","Documentary", "Drama", "Fantasy","Film-Noir", "Horror", "Musical", "Mystery","Romance","Sci-Fi", "Thriller", "War", "Western")

ratingmat <- sparseMatrix(ratings$UserID, ratings$MovieID, x=ratings$Rating)
dimnames(ratingmat) <- list(UserID = as.character(sort(unique(ratings$UserID))), MovieID = as.character(1:3952))
rm = as(ratingmat, "realRatingMatrix")

Rec.model<-Recommender(rm[1:6040], method = "UBCF")

new.user.id = nrow(users) + 1

shinyServer(function(input, output, session) {
    
    output$genres <- renderUI({
        selectInput("fav", "Genre:", choices = genre_list)
    })
    
    # show the books to be rated
    output$ratings <- renderUI({
        num_rows <- 20
        num_movies <- 6 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = top.rated$image_url[(i - 1) * num_movies + j], height = 150)),
                         div(style = "text-align:center", strong(top.rated$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", top.rated$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    # Calculate recommendations when the sbumbutton is clicked
    df <- eventReactive(input$btn, {
        withBusyIndicatorServer("btn", { # showing the busy indicator
            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            recommendation.list = top.weighted
            user.genre <- filter(recommendation.list, grepl(value_list$fav, Genres))
            
            user_results = (1:10)/10
            user_predicted_ids = 1:10
            recom_results <- data.table(Rank = 1:10, 
                                        MovieID = user.genre$MovieID[user_predicted_ids], 
                                        Title = user.genre$Title[user_predicted_ids], 
                                        Predicted_rating =  user_results)
            
        }) # still busy
        
    }) # clicked on button
    
    df2 <- eventReactive(input$btn2, {
        withBusyIndicatorServer("btn2", { # showing the busy indicator
            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            user_ratings <- get_user_ratings(value_list)
            user_ratings = cbind(rep(new.user.id, nrow(user_ratings)), user_ratings)
            user_ratings = cbind(user_ratings, rep(0, nrow(user_ratings)))
            colnames(user_ratings)[1] = "UserID"
            colnames(user_ratings)[4] = "Timestamp"
            rmat = ratings
            rmat = rbind(rmat, user_ratings)
            
            ratingmat <- sparseMatrix(rmat$UserID, rmat$MovieID, x=rmat$Rating)
            dimnames(ratingmat) <- list(UserID = as.character(sort(unique(rmat$UserID))), MovieID = as.character(1:3952))
            rm = as(ratingmat, "realRatingMatrix")
            
            recommended.items <- predict(Rec.model, rm[new.user.id,], n=10)
            top.10.recommended = as(recommended.items, "list")
            movies.indices = which(movies$MovieID %in% as.numeric(top.10.recommended[[1]]))
            
            recom_results <- data.table(Rank = 1:10,
                                        MovieID = movies.indices,
                                        Title = movies$Title[movies.indices],
                                        Predicted_rating =  0)
        })
    })
    
    output$recommendations <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df2()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies[recom_result$MovieID[(i - 1) * num_movies + j],]$image_url, height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies[recom_result$MovieID[(i - 1) * num_movies + j],]$Title)
                    )
                    
                )        
            }))) # columns
        }) # rows
    })
    
    # display the recommendations
    output$results <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df()

        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]),]$image_url, height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j]),]$Title)
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
}) # server function
