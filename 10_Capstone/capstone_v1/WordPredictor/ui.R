if (!require(shiny)) {
        install.packages("tm", shiny = "http://cran.us.r-project.org")
        require(shiny, quietly = TRUE)
}

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
                                                               value = "",
                                                               width = validateCssUnit("50%"),
                                                               placeholder = "Enter text in the box and click submit."
                                                               ),
                                                     submitButton("Submit"),
                                                     textOutput("predWord")
                                                     )
                                              )
                                      )
                              )
                     )
          )