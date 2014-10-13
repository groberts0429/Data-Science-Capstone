shinyUI(fluidPage (
  titlePanel("TextPredictor"),
  sidebarLayout(
    sidebarPanel(
      textInput("inputBoxId", "Input", value="<start>"), #textInput(inputId, label, value = "")
      br(),
      div(textOutput("outputBoxId", container = span), style=boxstyle)
    ),
    mainPanel(
      strong("Instructions"),
      br(),
      p("Type one, or two-word input in the 'Input' box.")
    )
  )
))
