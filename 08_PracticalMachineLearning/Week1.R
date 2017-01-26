install.packages("kernlab")
library(kernlab)

rm(list = ls())
#Question - can we use quantitative characteristic of email to classify as spam?
#Input-data
data(spam)
#view the features
head(spam)
#assemble the algorithm
plot(density(spam$your[spam$type == "nonspam"]), col = "blue", main = "", 
     xlab = "Frequency of 'your'")
lines(density(spam$your[spam$type == "spam"]), col = "red")
#add parameters
abline(v = 0.5, col = "green")
#evaluate through prediction
prediction <- ifelse(spam$your > 0.5, "spam", "nonspam")
table(prediction, spam$type)/length(spam$type)
# prediction      nonspam         spam
# nonspam 0.4590306455 0.1017170180
# spam    0.1469245816 0.2923277548

#add the .459 to .292 to get 0.751 - which is the accuracy
#right 45% of the timefor spam and 29% for nonspam

#in v out of sample errors
library(kernlab)
data(spam)
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size = 10), ]
spamLabel <- (smallSpam$type == "spam")*1+1
plot(smallSpam$capitalAve, col = spamLabel)

#build the rules
rule1 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.7] <- "spam"
        prediction[x < 2.40] <- "nonspam"
        prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
        prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
        return(prediction)
}
#this rule essentially calls out the spam outlier to make the training set perfect
table(rule1(smallSpam$capitalAve),smallSpam$type)
#            nonspam spam
# nonspam       5    0
# spam          0    5
#this is a perfectly fit model - for the TRAINING set
#apply a less-stringent rule
rule2 <- function(x){
        prediction <- rep(NA,length(x))
        prediction[x > 2.8] <- "spam"
        prediction[x <= 2.8] <- "nonspam"
        return(prediction)
}
table(rule2(smallSpam$capitalAve),smallSpam$type)
#             nonspam spam
# nonspam       5    1
# spam          0    4
#the second rule did not account for the outlier directly, so the model is not fit perfectly

#now test against all the data
#the overfitted rule
table(rule1(spam$capitalAve),spam$type)
#               nonspam spam
# nonspam    2141  588
# spam        647 1225
#647 and 588 are the errors
table(rule2(spam$capitalAve),spam$type)
# nonspam spam
# nonspam    2224  642
# spam        564 1171

sum(rule1(spam$capitalAve)==spam$type)
#[1] 3366
sum(rule2(spam$capitalAve)==spam$type)
#[1] 3395
#the more simplified rule performs better due to not being overfit - the overfitted
#model attended to the noise directly instead of focusing on the signal and automatically
#ignoring the noise by good model design

