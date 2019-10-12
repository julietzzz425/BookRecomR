library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
# install.packages(devtools)
# install.packages(slam)
library(devtools)
# install_github('stefanwilhelm/ShinyRatingInput', force = TRUE)
library(ShinyRatingInput)
library(shinyjs)
library(shinythemes)

library(sqldf)
library(dplyr)

getwd()
setwd("/home/yan176/R/MGMT 590/Shiny")
source('functions/helpers.R')
ui <-fluidPage(theme = shinytheme("superhero"),
               headerPanel('Book recommendation'),
               
               sidebarPanel(
                 
                 h4('Please enter 3 books you read and rate them'),
                 
                 textInput("name1", "Enter the first book's name"),
                 
                 selectInput('rating1', 'rate the first book', seq(1,5,by=1 )),
                 
                 textInput("name2", "Enter the second book's name"),
                 
                 selectInput('rating2', 'rate the second book', seq(1,5,by=1 )),
                 
                 textInput("name3", "Enter the third book's name"),
                 
                 selectInput('rating3', 'rate the third book', seq(1,5,by=1 )),
                 img(src='https://www.warehousedirectusa.com/media/catalog/product/cache/1/image/1100x/9df78eab33525d08d6e5fb8d27136e95/g/r/gram-9030-2.jpg', 
                     align = "left",
                     width = 200,
                     height = 150),
                 actionButton("go", "Click here to get your recommendations", class = "btn-warning",style='padding:4px; font-size:80%')
                 
                 
                 
               ),
               mainPanel(
                 
                 
                 # tags$style(),
                 fluidRow(
                   h3("The books you might like"),
                   tableOutput('results')
                 ),
                 
                 
                 
                 fluidRow(
                   h3("The detail of books you read"),
                   tableOutput("Detail")
                   
                 )
               )
               
)





# Server





# load functions
source('functions/helpers.R')

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measure
# read in data
books <- fread('data/books.csv')
ratings <- fread('data/ratings_cleaned.csv')

# reshape to books x user matrix 
ratingmat <- sparseMatrix(ratings$book_id, ratings$user_id, x=ratings$rating) # book x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))

server <-function(input, output) {
  bookid <- reactive({
    SelectedBook1 <- subset(books, title %in% input$name1)
    SelectedBook2 <- subset(books, title %in% input$name2)
    SelectedBook3 <- subset(books, title %in% input$name3)
    SelectedBook <- rbind(SelectedBook1,SelectedBook2, SelectedBook3)
  })
  Result<- eventReactive(input$go, {
    
    book_list <- c("The Great Gatsby","The Hobbit","The Catcher in the Rye")
    book_list <- as.list(book_list)
    SelectedBook <- subset(books, title %in% book_list)
    
    rating_list <- c(4,4,5)
    
    dat <- data.table(book_id = SelectedBook$book_id,
                      rating = rating_list)
    dat <- dat[!is.null(rating) & !is.na(book_id)]
    
    dat <- dat[rating > 0]
    
    
    
    user_ratings <- sparseMatrix(i = dat$book_id,
                                 j = rep(1, nrow(dat)),
                                 x = dat$rating,
                                 dims = c(nrow(ratingmat), 1)
    )
    rmat <- cbind(user_ratings, ratingmat)
    
    # get the indices of which cells in the matrix should be predicted
    # predict all books the current user has not yet rated
    items_to_predict <- which(rmat[, 1] == 0)
    prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
    
    # run the ubcf-alogrithm
    res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
    
    # sort, organize, and return the results
    user_results <- sort(res[, 1], decreasing = TRUE)[1:20]
    user_predicted_ids <- as.numeric(names(user_results))
    recom_results <- data.table(Rank = 1:20,
                                Book_id = user_predicted_ids,
                                Author = books$authors[user_predicted_ids],
                                Title = books$title[user_predicted_ids],
                                Predicted_rating =  user_results)
    
  })
  output$results <- renderTable({
    Result()
  })
  
  
  
  
  output$Detail <- renderTable({
    bookid()[,c(6,8,9,10,11,12)]
  })
}




shinyApp(ui = ui, server = server)


From: Wenhan Yan <yan176@purdue.edu>
  Sent: Friday, October 11, 2019 11:22 PM
To: Yuchen Li <li2114@purdue.edu>; Chunxuan Zhang <zhan2381@purdue.edu>
  Subject: Re: 
  
  
  
  From: Wenhan Yan
Sent: Friday, October 11, 2019 11:21 PM
To: Haowei Lai <lai68@purdue.edu>
  Subject: 
  
  library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
# install.packages(devtools)
# install.packages(slam)
library(devtools)
# install_github('stefanwilhelm/ShinyRatingInput', force = TRUE)
library(ShinyRatingInput)
library(shinyjs)
library(shinythemes)

library(sqldf)
library(dplyr)

getwd()
setwd("/home/yan176/R/MGMT 590/Shiny")
source('functions/helpers.R')
ui <-fluidPage(theme = shinytheme("superhero"),
               
               tags$head(tags$style(HTML("a {color: blue}"))),             
               
               headerPanel('Book recommendation'),
               
               sidebarPanel(
                 
                 h4('Please enter 3 books you read and rate them'),
                 
                 textInput("name1", "Enter the first book's name"),
                 
                 selectInput('rating1', 'rate the first book', seq(1,5,by=1 )),
                 
                 textInput("name2", "Enter the second book's name"),
                 
                 selectInput('rating2', 'rate the second book', seq(1,5,by=1 )),
                 
                 textInput("name3", "Enter the third book's name"),
                 
                 selectInput('rating3', 'rate the third book', seq(1,5,by=1 )),
                 
                 withBusyIndicatorUI(
                   actionButton("btn", "Click here to get your recommendations", class = "btn-warning",style='padding:4px; font-size:80%')
                 )
                 
                 
                 
               ),
               
               
               mainPanel(
                 
                 
                 # tags$style(),
                 fluidRow(
                   box(width = 6, title = "Here are books you might like", status = "info", solidHeader = TRUE,
                       div(class = "resultstable",
                           Output('results')
                       )
                   )
                   
                 ),
                 fluidRow(
                   useShinyjs(),
                   box(
                     width = 6, status = "info", solidHeader = TRUE,
                     title = "Detail of books you enter",
                     br(),
                     
                     br(),
                     tableOutput("Detail")
                     
                   )
                 )
               )
)





# Server

# read in data
books <- fread('data/books.csv')
ratings <- fread('data/ratings_cleaned.csv')





# reshape to books x user matrix 
ratingmat <- sparseMatrix(ratings$book_id, ratings$user_id, x=ratings$rating) # book x user matrix
ratingmat <- ratingmat[, unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(book_id = as.character(1:10000), user_id = as.character(sort(unique(ratings$user_id))))







# load functions
source('functions/helpers.R')

source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measure



server <-function(input, output) {
  Result<- eventReactive(input$btn, {
    book_list <- reactiveValuesToList(input)
    book_list <- as.list(book_list)
    SelectedBook <- subset(books, title %in% book_list)
    
    rating_list <- reactiveValuesToList(c(input$rating1, input$rating2,input$rating3))
    
    dat <- data.table(book_id = SelectedBook$book_id, 
                      rating = rating_list)
    dat <- dat[!is.null(rating) & !is.na(book_id)]
    
    dat <- dat[rating > 0]
    
    
    
    user_ratings <- sparseMatrix(i = dat$book_id, 
                                 j = rep(1, nrow(dat)),
                                 x = dat$rating, 
                                 dims = c(nrow(ratingmat), 1) 
    )
    rmat <- cbind(user_ratings, ratingmat)
    
    # get the indices of which cells in the matrix should be predicted
    # predict all books the current user has not yet rated
    items_to_predict <- which(rmat[, 1] == 0)
    prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))
    
    # run the ubcf-alogrithm
    res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 1000, FALSE, 2000, 1000)
    
    # sort, organize, and return the results
    user_results <- sort(res[, 1], decreasing = TRUE)[1:3]
    user_predicted_ids <- as.numeric(names(user_results))
    recom_results <- data.table(Rank = 1:3, 
                                Book_id = user_predicted_ids, 
                                Author = books$authors[user_predicted_ids], 
                                Title = books$title[user_predicted_ids], 
                                Predicted_rating =  user_results)
    
  })
  
  output$Detail <- renderTable(
    (SelectedBook[,c(1:3)])
  )
  
  
  
  
}



shinyApp(ui = ui, server = server)
