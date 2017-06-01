if (!require(shiny)) {
        install.packages("tm", shiny = "http://cran.us.r-project.org")
        require(shiny, quietly = TRUE)
}

if (!require(tidyverse)) {
        install.packages("tidyverse", repos = "http://cran.us.r-project.org")
        require(tidyverse, warn.conflicts = F)
}

if (!require(tm)) {
        install.packages("tm", repos = "http://cran.us.r-project.org")
        require(tm, quietly = TRUE)
}

#Load primary data
bigrams <- read_rds("final2.RData")
trigrams <- read_rds("final3.RData")
quadgrams <- read_rds("final4.RData")

#Create a shinyUI to allow users to enter the data and see the predicted word
  
  shinyUI(navbarPage("Word Predictor",
                     tabPanel("Documentation",
                              h2("Word Predictor"),
                              div("The Word Predictor Application allows users to enter text to see 
                                if the algorithm can correctly predict the next word in the sentence."),
                              br(),
                              div("The Prediction Generator uses a base corpus generated from 
                                  news, blog, and tweet text provided by SwiftKey as part of the Data
                                  Science capstone through Coursera. An algortithm is used to generate
                                  a frequency matrix for n-grams or word combinations. The likelihood
                                  of an n-gram appearing is used to determine which word should follow
                                  the user submitted n-gram.")
                              ),
                     tabPanel("Prediction Generator",
                              fluidPage(
                                      fluidRow(
                                              column(6,
                                                     "Word Predictor",
                                                     textInput(inputId = "screenInput",
                                                               label = "Enter text below:",
                                                               width = validateCssUnit("80%"),
                                                               placeholder = "Enter text in the box."
                                                               )
                                                     ),
                                              mainPanel(
                                                      h4("Predicted Word"),
                                                      textOutput("predWord")
                                                      
                                              )
                                              )
                                      )
                              )
                    
          )
  )