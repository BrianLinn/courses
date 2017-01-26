rm(list = ls())

#1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData <- data.frame(diagnosis, predictors)
testIndex <- createDataPartition(diagnosis,
                                 p = 0.5,
                                 list = FALSE)
training <- adData[-testIndex, ]
testing <- adData[testIndex, ]

#2
rm(list = ls())
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
data(concrete)

set.seed(1000)
inTrain <- createDataPartition(mixtures$CompressiveStrength,
                               p = 3/4)[[1]]
training <- mixtures[inTrain, ]
testing <- mixtures[-inTrain, ]
names <- names(mixtures)
names <- names[-9]

featurePlot(x = training[, names],
            y = training$CompressiveStrength,
            plot = "pairs")
#create the index
index <- seq_along(1:nrow(training))
qplot(CompressiveStrength, index, data = training)
#break CS into categories - hmisc package
cutCS <- cut2(training$CompressiveStrength, g = 4)
qplot(cutCS, index, data = training)
ggplot(data=training, aes(x=index, y=CompressiveStrength)) + geom_point() + 
        theme_bw()
ggplot(data=training, aes(x=index, y=cutCS)) + geom_point() + 
        theme_bw()
#plot with qplot
qplot(CompressiveStrength, Cement, data = training)
qplot(CompressiveStrength, BlastFurnaceSlag, data = training)
qplot(CompressiveStrength, FlyAsh, data = training)
qplot(CompressiveStrength, Water, data = training)
qplot(CompressiveStrength, Superplasticizer, data = training)
qplot(CompressiveStrength, CoarseAggregate, data = training)
qplot(CompressiveStrength, FineAggregate, data = training)
qplot(CompressiveStrength, Age, data = training)
#add color to the qplot
qplot(CompressiveStrength, Cement, colour = Age, data = training)
#add regression smoothers to the plot - ggplot2
qq <- qplot(CompressiveStrength, Cement, colour = Age, data = training)
qq + geom_smooth(method = "lm", formula = y ~ x)

#break wages into categories - hmisc package
cutCS <- cut2(training$CompressiveStrength, g = 4)
table(cutCS)
# cutWage
# [ 20.1, 92) [ 92.0,119) [119.1,318] 
# 702         721         679 

#There is a non-random pattern in the plot of the outcome versus index that does 
#not appear to be perfectly explained by any predictor suggesting a variable may be missing.

#3
remove(list = ls())
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

par(mfrow = c(1, 2))
hist(training$Superplasticizer)
hist(log(training$Superplasticizer))
range(training$Superplasticizer)
#There are values of zero so when you take the log() transform those values will be -Inf.

#4
rm(list = ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#create training set with only the "IL" variables
trainingSS <- training[, grep('^IL', names(adData))]
preProcSS <- preProcess(trainingSS,method="pca", thresh = 0.9)
preProcSS
# Created from 251 samples and 12 variables
# 
# Pre-processing:
#         - centered (12)
# - ignored (0)
# - principal component signal extraction (12)
# - scaled (12)
# 
# PCA needed 9 components to capture 90 percent of the variance

#5
rm(list = ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
#Create a training data set consisting of only the predictors with variable names 
#beginning with IL and the diagnosis
trainingSS <- training[, grep('^IL', names(adData))]
trainingAll <- data.frame(trainingSS, training$diagnosis)
testingSS <- testing[, grep('^IL', names(adData))]
testingAll <- data.frame(testingSS, testing$diagnosis)

#Build two predictive models, one using the predictors as they are and one using 
#PCA with principal components explaining 80% of the variance in the predictors. 
#Use method="glm" in the train function.

#predictive model with variables as is
modelFitAll <- train(trainingAll$training.diagnosis ~ .,method="glm", data=trainingAll)
#predictive model using principal component analysis with thresh set to 0.8
modelFitSS <- train(trainingAll$training.diagnosis ~ .,method="glm", 
                    preProcess = "pca",  data=trainingAll,
                    trControl=trainControl(preProcOptions=list(thresh=0.8)))

#store the predictions of the two models for the testing data
predictAll <- predict(modelFitAll, newdata = testingAll)
predictSS <- predict(modelFitSS, newdata = testingAll)

#produce confusion matrix
confusionMatrix(testingAll$testing.diagnosis, predict(modelFitAll, testingAll))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Impaired Control
# Impaired        2      20
# Control         9      51
# 
# Accuracy : 0.6463         
# 95% CI : (0.533, 0.7488)
# No Information Rate : 0.8659         
# P-Value [Acc > NIR] : 1.00000        
# 
# Kappa : -0.0702        
# Mcnemar's Test P-Value : 0.06332        
# 
# Sensitivity : 0.18182        
# Specificity : 0.71831        
# Pos Pred Value : 0.09091        
# Neg Pred Value : 0.85000        
# Prevalence : 0.13415        
# Detection Rate : 0.02439        
# Detection Prevalence : 0.26829        
# Balanced Accuracy : 0.45006        
# 
# 'Positive' Class : Impaired 
confusionMatrix(testingAll$testing.diagnosis, predict(modelFitSS, testingAll))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Impaired Control
# Impaired        3      19
# Control         4      56
# 
# Accuracy : 0.7195          
# 95% CI : (0.6094, 0.8132)
# No Information Rate : 0.9146          
# P-Value [Acc > NIR] : 1.000000        
# 
# Kappa : 0.0889          
# Mcnemar's Test P-Value : 0.003509        
#                                           
#             Sensitivity : 0.42857         
#             Specificity : 0.74667         
#          Pos Pred Value : 0.13636         
#          Neg Pred Value : 0.93333         
#              Prevalence : 0.08537         
#          Detection Rate : 0.03659         
#    Detection Prevalence : 0.26829         
#       Balanced Accuracy : 0.58762         
#                                           
#        'Positive' Class : Impaired
# Accuracy : 0.6463 and # Accuracy : 0.7195   