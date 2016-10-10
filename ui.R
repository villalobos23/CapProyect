#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Ngram Language model Prototype"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      titlePanel("Input Panel"),
      textInput("inputText", "Write your text here", value = "this is a "),
      actionButton("enterButton", "Predict"),
      p("Click the button to get Suggestions for the text."),
      HTML("<p>Developed by <a href='https://villalobos23.github.io'>Luis Villalobos</a> - 2016</p>"),
      titlePanel("Instructions"),
      p("The instructions are simple, you enter the text you want to predict and press the button that executes the prediction process"),
      p("If you see sntcst or sntcnd as the result of the algorithm, it means that the most likely operation is so start a new sentence or to end the current one"),
      p("To predict the current word being typed dont leave a whitespace in the end."),
      p("And if you want to predict the next whole word leave a whitespace in the end")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      titlePanel("Suggestions"),
       verbatimTextOutput("suggestions"),
      titlePanel("Unsuccessful prediction"),
       verbatimTextOutput("frame"),
      titlePanel("Possible new Sentence"),
       verbatimTextOutput("newSentence")
    )
  )
))

#http://brooksandrew.github.io/simpleblog/articles/deploying-apps-with-shinyapps-io/
#http://stackoverflow.com/questions/31557428/r-load-only-once-a-rdata-in-a-deployed-shinyapp
