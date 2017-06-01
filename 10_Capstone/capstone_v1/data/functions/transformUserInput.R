#Clear the global environment and detach all packages
#source("./data/functions/detachAllPackages.R")
#detachAllPackages()
#remove(list = ls())
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

#Load primary data
bigrams <- read_rds("final2.RData")
trigrams <- read_rds("final3.RData")
quadgrams <- read_rds("final4.RData")

cleanData <- function(userInput){
        bigrams <- read_rds("final2.RData")
        trigrams <- read_rds("final3.RData")
        quadgrams <- read_rds("final4.RData")
        textInput <- userInput %>%
                tolower() %>% 
                removePunctuation() %>% 
                removeNumbers() %>% 
                stripWhitespace() %>% 
                txt.to.words.ext()
        #Return the clean text
        print(textInput)
}