#Quiz question 1
file.info(fileList)[1]/1048576
#200MB or blogs.txt

#2
length(readLines(fileList[[3]][1]))
#over 2 million

length(which.max(nchar(readLines(fileList[[3]][1]))))

fileList[[1]][1]

#3
blogf <- file(fileList[[1]][1], "r")
lineblog <- readLines(blogf)
ncharblog <- nchar(lineblog)
maxblog <- max(ncharblog)
close(blogf)

newsf <- file(fileList[[2]][1], "r")
linenews <- readLines(newsf)
ncharnews <- nchar(linenews)
maxnews <- max(ncharnews)
close(newsf)

twitterf <- file(fileList[[3]][1], "r")
linetwitter <- readLines(twitterf)
nchartwitter <- nchar(linetwitter)
maxtwitter <- max(nchartwitter)
close(twitterf)

data.frame(maxblog, maxnews, maxtwitter)
#over 40 thousand in the blog file

#4
patternL = "love"
love <- grep(linetwitter, pattern = patternL, value = FALSE) 

patternH = "hate"
hate <- grep(linetwitter, pattern = patternH, value = FALSE)
length(love)/length(hate)
#4.108592

#5
patternB = "biostat"
biostat <- grep(linetwitter, pattern = patternB, value = TRUE)
#they need to study for the biostats test

#6
patternX <- "A computer once beat me at chess, but it was no match for me at kickboxing"
X <- grep(linetwitter, pattern = patternX)
length(X)
