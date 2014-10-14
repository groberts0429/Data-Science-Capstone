library(shiny)
shinyUI(fluidPage(
  titlePanel("TextPredictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Type a one or two word input into the input box and press 'Enter'."),
      
      textInput("word", "Input", value="<start>"),
      textOutput("text1"),
      textOutput("text2"),
      textOutput("text3"),
      textOutput("word")
    ),
    
    mainPanel( #no main panel
    )
  )
))