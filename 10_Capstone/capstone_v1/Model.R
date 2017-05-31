#Clear the global environment and detach all packages
source("./data/functions/detachAllPackages.R")
detachAllPackages()
remove(list = ls())

#Download if necessary and set to required the libraries needed for the code
if (!require(tidyverse)) {
        install.packages("tidyverse", repos = "http://cran.us.r-project.org")
        require(tidyverse, quietly = TRUE)
}


if (!require(tm)) {
        install.packages("tm", repos = "http://cran.us.r-project.org")
        require(tm, quietly = TRUE)
}

if (!require(RWeka)) {
        install.packages("RWeka", repos = "http://cran.us.r-project.org")
        require(RWeka, quietly = TRUE)
}

#Download the data - store in the working directory in a folder called 'data'
if(!file.exists("./data/source/Coursera-SwiftKey.zip")){
        res <- tryCatch(download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                                      destfile="./data/source/Coursera-SwiftKey.zip",
                                      method="auto"),
                        error=function(e) 1)
        if(res!=1) unzip("./data/source/Coursera-SwiftKey.zip")
}

#Read the files in
blogs <- read_lines("./data/source/final/en_US/en_US.blogs.txt")
news <- read_lines("./data/source/final/en_US/en_US.news.txt")
twitter <- read_lines("./data/source/final/en_US/en_US.twitter.txt")

#First the data is sampled to increase performance
#Store the number of documents to retreive from each data set
redFac <- 10000

smlBlogs <- blogs[sample(1:length(blogs),redFac)]
smlNews <- news[sample(1:length(news),redFac)]
smlTwitter <- twitter[sample(1:length(twitter),redFac)]
sourceDocs <- c(smlTwitter,smlNews,smlBlogs)


#Save the combined files and read in for analysis
writeLines(sourceDocs, "./data/source/sourceDocs.txt")

removeMe <- c("blogs", "news", "twitter", "smlBlogs", "smlNews", "smlTwitter",
              "sourceDocs", "redFac", "removeMe")
remove(list = removeMe)

#Read the source file in for further analysis
sourceConnection <- file("./data/source/sourceDocs.txt")
source <- readLines(sourceConnection)
close(sourceConnection)

#Create the Corpus
cleanCorp <- Corpus(VectorSource(source))
removeMe <- c("sourceConnection", "source", "removeMe")
remove(list = removeMe)

#Cleanup of the corpus
#Store the profane words to remove
profanityURL <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
profanity <- read_lines(profanityURL)
#Convert to utf-8
cleanCorp <- tm_map(cleanCorp,
                      content_transformer(function(x) 
                              iconv(x, to="UTF-8", sub="byte")))
#Convert to lower case
cleanCorp <- tm_map(cleanCorp, content_transformer(tolower))
#Remove punctuation
cleanCorp <- tm_map(cleanCorp, content_transformer(removePunctuation))
#Remove numbers
cleanCorp <- tm_map(cleanCorp, content_transformer(removeNumbers))

#Remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
cleanCorp <- tm_map(cleanCorp, content_transformer(removeURL))
#Remove white space
cleanCorp <- tm_map(cleanCorp, content_transformer(stripWhitespace))
#Remove stop and profane words
cleanCorp <- tm_map(cleanCorp, removeWords, c(profanity))
#Stem the document
#cleanCorp <- tm_map(cleanCorp, content_transformer(stemDocument))
#strip whitespace again after stemming
cleanCorp <- tm_map(cleanCorp, content_transformer(stripWhitespace))

#Save the clean corpus as an R data file
saveRDS(cleanCorp, file = "./data/corpus/corpus.RDS")

removeMe <- c("removeMe", "profanityURL", "profanity", "cleanCorp", "removeURL")
remove(list = removeMe)

#Build the n-grams and RDS files for use in the shiny app and predictor
#Create the tokenizer function
gramTokenizer <- function(corp, grams) {
        #Call the weka control and set the max and min grams to input grams - use
        #the default delimiters
        gramFunc <- NGramTokenizer(corp, 
                                        Weka_control(min = grams, max = grams, 
                                                     delimiters = " \\r\\n\\t.,;:\"()?!"))
        #Covnert the tokenized n grams to a data frame and sort descending by frequency
        gramFunc <- data.frame(table(gramFunc))
        gramFunc <- gramFunc[order(gramFunc$Freq, 
                                             decreasing = TRUE),]
        #Assign appropriate names
        colnames(gramFunc) <- c("Text","Frequency")
        gramFunc
}

#Read in the corpus
finalCorp <- readRDS("./data/corpus/corpus.RDS")

corpDF <- unlist(finalCorp) %>% as_tibble()

#Unigram
unigrams <- gramTokenizer(corpDF, 1)
saveRDS(unigrams, file = "./data/grams/unigram.RDS")

#Bigram
bigrams <- gramTokenizer(corpDF, 2)
saveRDS(bigrams, file = "./data/grams/bigram.RDS")

#Trigram
trigrams <- gramTokenizer(corpDF, 3)
saveRDS(trigrams, file = "./data/grams/trigram.RDS")

#Quadgram
quadgrams <- gramTokenizer(corpDF, 4)
saveRDS(quadgrams, file = "./data/grams/quadgram.RDS")

removeMe <- c("removeMe", "finalCorp", "unigrams", "bigrams", "trigrams", "quadgrams")
removeMe

#Read in the saved data frame for quadgrams
quadgramFinal <-  read_rds("./data/grams/quadgram.rds")

#separate the data frame into 4 columns - one per n-gram
final4 <- separate(data = quadgramFinal, 
                col = Text, 
                into = c("Unigram", "Bigram", "Trigram", "Quadgram"), 
                sep = "[:space:]",
                remove = TRUE,
                convert = FALSE)
saveRDS(final4, "./data/grams/final4.RData")
remove(quadgramFinal, final4)

#Read in the saved data frame for trigrams
trigramFinal <-  read_rds("./data/grams/trigram.rds")

#separate the data frame into 3 columns - one per n-gram
final3 <- separate(data = trigramFinal, 
                   col = Text, 
                   into = c("Unigram", "Bigram", "Trigram"), 
                   sep = "[:space:]",
                   remove = TRUE,
                   convert = FALSE)
saveRDS(final3, "./data/grams/final3.RData")
remove(trigramFinal, final3)

#Read in the saved data frame for bigrams
bigramFinal <-  read_rds("./data/grams/bigram.rds")

#separate the data frame into 4 columns - one per n-gram
final2 <- separate(data = bigramFinal, 
                   col = Text, 
                   into = c("Unigram", "Bigram"), 
                   sep = "[:space:]",
                   remove = TRUE,
                   convert = FALSE)
saveRDS(final2, "./data/grams/final2.RData")
remove(bigramFinal, final2)