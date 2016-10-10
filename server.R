#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
if(!exists("getNextWord", mode="function")) source("predictionModel.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
   
  getTextBox <- eventReactive(input$enterButton,{
    input$inputText
  })
  
  unknowns <- eventReactive(input$enterButton,{
    wrongs
  })
  
  predictedWord <<- data.table()
  
  output$suggestions <- renderText({
    predictedWord <<- getNextWord(getTextBox())
    paste(predictedWord$word,collapse = ", ")
  })
  
  output$frame <- renderText({
    paste("Amount of Unknowns detected ",unknowns())
  })
  
  output$newSentence <- renderText({
    if (grepl("[[:blank:]]$",getTextBox())){
      paste(getTextBox(),head(predictedWord,1)$word,collapse = "")
    }else{
      transitory <- gsub("\\s*\\w*$","",getTextBox()) 
      paste(transitory,head(predictedWord,1)$word,collapse = "")
    }
  })
  
})
