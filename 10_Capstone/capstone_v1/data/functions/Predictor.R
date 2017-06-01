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

bigrams <- read_rds("final2.RData")
trigrams <- read_rds("final3.RData")
quadgrams <- read_rds("final4.RData")

#Predictor Function
predictor <- function(textLength,textInput){
        bigrams <- read_rds("final2.RData")
        trigrams <- read_rds("final3.RData")
        quadgrams <- read_rds("final4.RData")
        #Edit the text entered by the user to ensure that only three or less terms
        #are passed to the predictor
        
        #Use with Quadgrams
        #If length of text entered is greater than 3 - extract only the last 3 words
        if (textLength >= 3) {
                textInput <- textInput[(textLength - 2): textLength] 
                
        }
        
        #Use with Trigrams
        #If the word count is exactly 2, add NA to the first gram and use the 
        #two words to bring the input to 3 character strings
        else if(textLength == 2) {
                textInput <- c(NA, textInput)   
        }
        
        #Use with Bigrams
        #If word count is not greater than three or equal to 2 (so 1) then use NA for
        #the first two grams and the input as the third gram
        else {
                textInput <- c(NA,NA,textInput)
        }
        
        #First try entering the text into the quadgram 
        #If there is an NA in one of the three text fields being passed the 
        #function will result in an NA
        prediction <- as.character(quadgrams[quadgrams$Unigram == textInput[1] & 
                                                 quadgrams$Bigram == textInput[2] & 
                                                 quadgrams$Trigram == textInput[3],][1,]$Quadgram)

        #If the function resulted in an NA using the quadgrams, then try the trigrams. 
        #This too will result in NA if NA is passed from the input
        if(is.na(prediction)) {
                prediction <- as.character(trigrams[trigrams$Unigram == textInput[2] &
                                                        trigrams$Bigram == textInput[3],][1,]$Trigram)
                
                #Finally resort to the bigrams if only one word was passed as input
                if(is.na(prediction)) {
                        prediction <- as.character(bigrams[bigrams$Unigram==textInput[3],][1,]$Bigram)
                }
        }
        
        #Return the predicted word
        print(prediction)
        
}