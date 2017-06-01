#Clear the global environment and detach all packages
source("./data/functions/detachAllPackages.R")
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

if (!require(stylo)) {
        install.packages("stylo", repos = "http://cran.us.r-project.org")
        require(stylo, quietly = TRUE)
}

cleanData <- function(userInput){
        textInput <- userInput %>%
                tolower() %>% 
                removePunctuation() %>% 
                removeNumbers() %>% 
                stripWhitespace() %>% 
                txt.to.words.ext()
        
        print(textInput)
}

textInput <- cleanData(userInput = userInput)

textLength <- length(textInput)