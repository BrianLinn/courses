#Clear the global environment and detach all packages
source("detachAllPackages.R")
detachAllPackages()
remove(list = ls())

if (!require(tidyverse)) {
        install.packages("tidyverse", repos = "http://cran.us.r-project.org")
        require(tidyverse, warn.conflicts = F)
}

if (!require(tm)) {
        install.packages("tm", repos = "http://cran.us.r-project.org")
        require(tm, quietly = TRUE)
}

if (!require(shiny)) {
        install.packages("tm", shiny = "http://cran.us.r-project.org")
        require(shiny, quietly = TRUE)
}

#Load primary data
bigrams <- read_rds("final2.RData")
trigrams <- read_rds("final3.RData")
quadgrams <- read_rds("final4.RData")

#Load in the requisite functions

#User Input Transformer - used to create textInput and textLength
source("transformUserInput.R")

#Word Predictor
source("Predictor.R")

#Create the Shiny server function that will return the predicted word
shinyServer(function(input, output){
        #Create a reactive function that looks for changes in the screen input
        predWord <- reactive({
                userInput <- input$screenInput
                textInput <- cleanData(userInput = userInput)
                textLength <- length(textInput)
                predict <- predictor(textLength = textLength,
                                     textInput = textInput)
        })
        
        output$predWord <- renderText(predWord())
})