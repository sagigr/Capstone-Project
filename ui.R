
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(markdown)
shinyUI(
  fluidPage(
    navbarPage("IMDb Movies Data Explorer", 
               tabPanel("Explore the Data",
                        sidebarPanel(
                          sliderInput("rating", 
                                      "IMDb Rating:", 
                                      min = 1,
                                      max = 10,
                                      step= 0.1,
                                      value = c(7, 10)),
                          sliderInput("years", 
                                      "Years:", 
                                      min = 1893,
                                      max = 2005,
                                      value = c(1990, 2005)),
                          sliderInput("votes", 
                                      "Number of Votes:", 
                                      min = 5,
                                      max = 157608,
                                      value = c(10000, 157608)),
                          selectInput("mpaa", label = h3("MPAA Rating:"), 
                                      choices = list("","NC-17",
                                                     "PG", "PG-13", "R"), selected = ""),
                          checkboxGroupInput("genres", 
                                             label = h3("Genres:"), 
                                             choices = list("Action", "Animation",
                                                            "Comedy", "Drama",
                                                            "Documentary", "Romance",
                                                            "Short")
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            # Data 
                            tabPanel(p(icon("table"), "Dataset"),
                                     dataTableOutput(outputId="dTable")
                            ) # end of "Dataset" tab panel
                            
                          ) 
                          
                        )
               ),
               # end of "Explore Dataset" tab panel
               
               tabPanel("About",
                        mainPanel(
                          includeMarkdown("about.md")
                        )
               ) # end of "About" tab panel
    )
  )
)
