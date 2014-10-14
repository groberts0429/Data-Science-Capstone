library(shiny)

##### Load the Prediction Matrix ####
############# Roses are Red Dataset #############
#load(file="C:/TextPredictor/rosemce.rda")
#load(file="C:/TextPredictor/ubothe.rda")
#load(file="C:/TextPredictor/firste.rda")
#load(file="C:/TextPredictor/seconde.rda")
#load(file="C:/TextPredictor/bissumse.rda")

############ 1000 Tweets and all Other Datasets ###########
load(file="C:/Data-Science-Capstone/mbi.rda")
load(file="C:/Data-Science-Capstone/uboth.rda")
load(file="C:/Data-Science-Capstone/first.rda")
load(file="C:/Data-Science-Capstone/second.rda")
load(file="C:/Data-Science-Capstone/bissums.rda")


#### Prediction Function ####
predict3 <- function(matrix, uboth, begin, last,  rowsums, input) { #rowsums for the ngrams not the matrix!  
  #if (is.null(input)) { input = "<start>" }
  ufirst  <- unique(begin)
  usecond <- unique(last)
  row = grep(paste("^", input, "$", sep=""), uboth)
  ipositions = grep(paste("^", input, "$", sep=""), begin)
  frequencies = rowsums[ipositions]
  words = last[ipositions]
  nindex1 = which.max(frequencies); option1 = words[nindex1]; frequencies = frequencies[-nindex1]; words = words[-nindex1]
  nindex2 = which.max(frequencies); option2 = words[nindex2]; frequencies = frequencies[-nindex2]; words = words[-nindex2]
  nindex3 = which.max(frequencies); option3 = words[nindex3]
  return(c(option1, option2, option3))
}

shinyServer(
  function(input, output) {
#    dataInput <- reactive({
#      inputword = input$var
#    })
    observe({
      param <- input$word
#     prediction <- predict3(rosemce, ubothe, firste, seconde, bissumse, input$word)
      prediction <- predict3(mbi, uboth, first, second, bissums, input$word)
      output$text1 <- renderText({ 
        paste("Top Prediction:    ", prediction[1])
      })
      output$text2 <- renderText({ 
        paste("Second Prediction: ", prediction[2])
      })
      output$text3 <- renderText({ 
        paste("Third Prediction:  ", prediction[3])
      })
      
    })    
  }
)