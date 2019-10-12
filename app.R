
rsconnect::setAccountInfo(name='boilergoogler', 
                          token='6AA568BD153598B61F02060D9E48F5CB', 
                          secret='1Z+UYF/gvK6JYS6OodNQLIe5oICqaH5xWI4pCiuJ')

library(rmarkdown)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(devtools)
library(shinyjs)
library(shinythemes)

library(sqldf)
library(dplyr)
library(rsconnect)
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
    user_results <- sort(res[, 1], decreasing = TRUE)[1:3]
    user_predicted_ids <- as.numeric(names(user_results))
    recom_results <- data.table(Rank = 1:3,
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

