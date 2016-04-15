
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(markdown)
shinyUI(   fluidPage(
# theme = "bootstrap.css",
  navbarPage("Next Word Prediction App", 
             tabPanel("Word Prediction",  
                      mainPanel(
        h3("Start typing your text:"),
        textInput("inputTxt", "", width = '55%'),
        actionButton("reset", "Clear"),
        h4("Is the next required word here? Pick a suggestion, or continue typing."),
        uiOutput("words")
                               )
                     ),           
               tabPanel("About",
                     mainPanel(
                       includeMarkdown("about.md")
                               )
                        )
              )
                      )
         )
