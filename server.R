library(shiny)
library(dplyr)
library(ggplot2movies)
# Shiny server
shinyServer(
  function(input, output) {
    
    filterByInputCriteria <- function(dt, 	minYear, maxYear, 
                                      minRating, maxRating, 
                                      minBudget, maxBudget, 
                                      minVote, maxVote, 
                                      mpaas, genres) {
      
      result <- dt %>% filter(year >= minYear 
                              ,year <= maxYear
                              ,rating >= minRating, rating <= maxRating
                              ,votes >= minVote, votes <= maxVote
                              ,mpaa %in% mpaas
      )
      for (g in genres) {
        result <- result %>% filter(result[,g] == 1)
      }
      
      return(result)
    }
    
    # Prepare dataset
    dataTable <- reactive({
      filterByInputCriteria(movies, input$years[1], 
                            input$years[2], input$rating[1],
                            input$rating[2], input$budget[1],
                            input$budget[2], input$votes[1],
                            input$votes[2], input$mpaa, input$genres)
    })
    
    # Render movies table
    output$dTable <- renderDataTable(dataTable(), options = list(columnDefs = list(list(visible=FALSE, targets=c(6:15,17:23)))))    
    # end of function(input, output)
  })
