#1
remove(list = ls())
library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e10171)

#1
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)
library(randomForest)
fitRf <- train(y ~ ., method = "rf", data = vowel.train)
fitGbm <- train(y ~ ., method = "gbm", data = vowel.train, verbose = F)

pred1 <- predict(fitRf, vowel.test)
pred2 <- predict(fitGbm, vowel.test)

#plot the predictions
qplot(pred1, pred2, colour = y, data = vowel.test)

#combine the predictors into one model
predDF <- data.frame(pred1, pred2, y = vowel.test$y)
combModFit <- train(y ~ ., method = "gam", data = predDF)
combPred <- predict(combModFit, predDF)

#predict accuracy of model with confusion matrix
#random forest accuracy
round(confusionMatrix(pred1, vowel.test$y)[[3]][[1]], 4) #0.6147
#gbm accuracy
round(confusionMatrix(pred2, vowel.test$y)[[3]][[1]], 4) #0.5368
#combined predictor accuracy
round(confusionMatrix(pred1, pred2)[[3]][[1]], 4) #0.6797


#2
remove(list = ls())
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
dim(training) #251 31
dim(testing) #82 31


set.seed(62433)
modRf <- train(diagnosis ~ ., data = training, method = "rf")
set.seed(62433)
modGbm <- train(diagnosis ~ ., data = training, method = "gbm", verbose = F)
set.seed(62433)
modLda <- train(diagnosis ~ ., data = training, method = "lda")

predRf <- predict(modRf, testing)
predGbm <- predict(modGbm, testing)
predLda <- predict(modLda, testing)

#combine the predictors into one model
predDF <- data.frame(predRf, predGbm, predLda, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)

#random forest accuracy
round(confusionMatrix(predRf, testing$diagnosis)[[3]][[1]], 4) #0.7683
#gbm accuracy
round(confusionMatrix(predGbm, testing$diagnosis)[[3]][[1]], 4) #0.8049
#gbm accuracy
round(confusionMatrix(predLda, testing$diagnosis)[[3]][[1]], 4) #0.7683
#combined predictor accuracy
round(confusionMatrix(combPred, predDF$diagnosis)[[3]][[1]], 4) #0.8171
#staked accuracy is better than all three other predictors

#3
remove(list = ls())
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
names(concrete)
set.seed(233)
modFit <- train(CompressiveStrength ~ ., data = training, method = "lasso")

pred1 <- predict(modFit, testing)
?plot.enet
plot.enet(modFit$finalModel, xvar = "penalty", use.color = TRUE)
modFit$finalModel$beta.pure
#cement moves to zero last

#3
remove(list = ls())
library(lubridate) # For year() function below
dat = read.csv("./data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

library(forecast)

modFit <- bats(tstrain)
n <- length(testing$visitsTumblr)
fCast <- forecast(modFit, n, level = 95)
plot(fCast)
?forecast
sum(fCast$lower < testing$visitsTumblr & testing$visitsTumblr < fCast$upper)/length(testing$visitsTumblr)
#0.9617021
#number in 95CI
sum(fCast$lower < testing$visitsTumblr & testing$visitsTumblr < fCast$upper)


#4
remove(list = ls())
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
?svm
library(e1071)
modFit <- svm(CompressiveStrength ~ ., data = training)
pred <- predict(modFit, testing)

error <- pred - testing$CompressiveStrength
MSE <- mean(error^2)
RMSE <- sqrt(MSE)
RMSE
#6.715009
or
accuracy(pred, testing$CompressiveStrength)
# ME     RMSE      MAE       MPE     MAPE
# Test set 0.1682863 6.715009 5.120835 -7.102348 19.27739