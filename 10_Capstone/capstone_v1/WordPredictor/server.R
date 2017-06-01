#Clear the global environment and detach all packages
source("./data/functions/detachAllPackages.R")
detachAllPackages()
remove(list = ls())
source("Predictor.R")
getwd()
#Download if necessary and set to required the libraries needed for the code
if (!require(tidyverse)) {
        install.packages("tidyverse", repos = "http://cran.us.r-project.org")
        require(tidyverse, quietly = TRUE)
}

if (!require(shiny)) {
        install.packages("shiny", repos = "http://cran.us.r-project.org")
        require(shiny, quietly = TRUE)
}

#Load primary data
bigrams <- read_rds("./data/grams/final2.RData")
trigrams <- read_rds("./data/grams/final3.RData")
quadgrams <- read_rds("./data/grams/final4.RData")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  
})
