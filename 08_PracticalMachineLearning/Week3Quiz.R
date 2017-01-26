#1
remove(list = ls())
library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
data(segmentationOriginal)

inTrain <- segmentationOriginal$Case == "Train"
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

set.seed(125)
modFit <- train(Class ~ ., method = 'rpart', data = training)
fit    <- train(Class ~ ., data=training, method='rpart')

modFit$finalModel

library(rattle)
fancyRpartPlot(modFit$finalModel)

# Show decision tree: PS, WS, PS, Not possible to predict.
plot(modFit$finalModel, uniform=T)
text(modFit$finalModel, cex=0.8)
#PS, WS, PS, Not possible to predict.

#2
remove(list = ls())
#If K is small in a K-fold cross validation is the bias in the estimate of 
#out-of-sample (test set) accuracy smaller or bigger? 
# Bias is larger
#If K is small is the variance in the estimate of out-of-sample (test set) 
#accuracy smaller or bigger?
# Variance is smaller
# Is K large or small in leave one out cross validation?
#K = sample size in leave one out cross validation

#3
remove(list = ls())
library(pgmm)
data(olive)
olive$Area
olive <- olive[, -1]

modFit <- train(Area ~ ., data = olive, method = 'rpart')
fancyRpartPlot(modFit$finalModel)

predict(modFit, newdata = as.data.frame(t(colMeans(olive))))
# 2.783282

#4
rm(list = ls())
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
#Coronary Heart Disease (chd) as the outcome and age at onset, 
#current alcohol consumption, obesity levels, cumulative tabacco, 
#type-A behavior, and low density lipoprotein cholesterol 
fit <-          train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                        data = trainSA, method = "glm", family = "binomial")

predictTrain <- predict(fit, newdata = trainSA)
predictTest <- predict(fit, newdata = testSA)

missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predictTrain) #0.2727273 Train
missClass(testSA$chd, predictTest) #0.3116883 Test

#5
rm(list= ls())
library(ElemStatLearn)
data("vowel.train")
data("vowel.test")
head(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
library(randomForest)
fit <- randomForest(y ~ ., data = vowel.train)
library(caret)
?varImp 
max(varImp(fit))
min(varImp(fit))
range(varImp(fit))
order(varImp(fit), decreasing = TRUE)
#2  1  5  6  8  4  9  3  7 10