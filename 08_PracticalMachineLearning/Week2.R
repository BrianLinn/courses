install.packages("caret")
install.packages("RANN")
library(caret)
library(kernlab)
library(RANN)
data(spam)

#create a data partition for the testing and training set
#example of data sclicing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)

#fit a model
set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
#predict type using all of the other variables
modelFit
# Generalized Linear Model 
# 
# 3451 samples
# 57 predictor
# 2 classes: 'nonspam', 'spam' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
# Resampling results:
#         
#         Accuracy   Kappa    
# 0.9170281  0.825581

modelFit$finalModel

predictions <- predict(modelFit, newdata = testing)

#determine acuracy of model with confusion matrix
confusionMatrix(predictions, testing$type)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction nonspam spam
# nonspam     646   45
# spam         51  408
# 
# Accuracy : 0.9165         
# 95% CI : (0.899, 0.9319)
# No Information Rate : 0.6061         
# P-Value [Acc > NIR] : <2e-16         
# 
# Kappa : 0.8256         
# Mcnemar's Test P-Value : 0.6098         
#                                          
#             Sensitivity : 0.9268         
#             Specificity : 0.9007         
#          Pos Pred Value : 0.9349         
#          Neg Pred Value : 0.8889         
#              Prevalence : 0.6061         
#          Detection Rate : 0.5617         
#    Detection Prevalence : 0.6009         
#       Balanced Accuracy : 0.9137         
#                                          
#        'Positive' Class : nonspam 



#data slicing using k-folds - used for cross-validation
set.seed(32323)
folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
# Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
# 4141   4140   4141   4142   4140   4142   4141   4141   4140   4141 
folds[[1]][1:10]
folds2 <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = FALSE)
sapply(folds2, length)
# Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
# 460    459    460    460    460    460    460    460    461    461
folds2[[1]][1:10]


#resampling
folds3 <- createResample(y = spam$type, times = 10, list = TRUE)
sapply(folds3, length)
# Resample01 Resample02 Resample03 Resample04 Resample05 Resample06 Resample07 Resample08 Resample09 
# 4601       4601       4601       4601       4601       4601       4601       4601       4601 
# Resample10 
# 4601 
folds3[[1]][1:10]
#[1]  2  3  5  6  6  6  8  8  8 11

#time slices
set.seed(32323)
tme <- 1:1000
folds4 <- createTimeSlices(y = tme, initialWindow = 20, horizon = 10)
names(folds4)
folds4$train[[3]]
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

#Training Options
rm(list = ls())
library(caret)
library(kernlab)
data(spam)

#create a data partition for the testing and training set
#example of data sclicing
inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]
dim(training)
modelFit <- train(type ~ ., data = training, method = "glm")
args(train.default)
#training options
# Continous outcomes:
# RMSE = Root mean squared error
# RSquared = R2 from regression models
# Categorical outcomes:
# Accuracy = Fraction correct
# Kappa = A measure of concordance
args(trainControl)
# trainControl resampling
# method
#       boot = bootstrapping
#       boot632 = bootstrapping with adjustment
#       cv = cross validation
#       repeatedcv = repeated cross validation
#       LOOCV = leave one out cross validation
# number
#       For boot/cross validation
#       Number of subsamples to take
# repeats
#       Number of times to repeate subsampling
#       If big this can slow things down

#Plotting predictors
rm(list = ls())
library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
library(gridExtra)
#load the data 
data(Wage)
summary(Wage)
dim(Wage)
#[1] 3000   12

#create the test and train data sets
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]
dim(training) #3000*.7 = 2100
#[1] 2102   12
dim(testing) #3000 * .3 = 900
#[1] 898  12 

#plot the predictors with caret package
featurePlot(x = training[, c("age", "education", "jobclass")],
            y = training$wage,
            plot = "pairs")
#outcome is the wage with all of the predictors - age education and jobclass

#plot with qplot
qplot(age, wage, data = training)
#add color to the qplot
qplot(age, wage, colour = jobclass, data = training)
#add regression smoothers to the plot - ggplot2
qq <- qplot(age, wage, colour = education, data = training)
qq + geom_smooth(method = "lm", formula = y ~ x)

#break wages into categories - hmisc package
cutWage <- cut2(training$wage, g = 3)
table(cutWage)
# cutWage
# [ 20.1, 92) [ 92.0,119) [119.1,318] 
# 702         721         679 

#boxplots with cut2
p1 <- qplot(cutWage, age, data = training, fill = cutWage,
            geom = c("boxplot"))
p1
#now overlay points on the plot - use gridExtra for grid.arrange
p2 <- qplot(cutWage, age, data = training, fill = cutWage,
            geom = c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol = 2)


#tables
t1 <- table(cutWage, training$jobclass)
t1
# cutWage       1. Industrial 2. Information
# [ 20.1, 92)           448            254
# [ 92.0,119)           360            361
# [119.1,318]           262            417

#more industrial jobs with lower wages - opposite trend for information jobs
#to get the proportions in the groups
prop.table(t1, 1) #1 for rows
# cutWage       1. Industrial 2. Information
# [ 20.1, 92)     0.6381766      0.3618234
# [ 92.0,119)     0.4993065      0.5006935
# [119.1,318]     0.3858616      0.6141384
#larger proportion of low wage jobs are industrial

#use density plots to see all predictors at once
qplot(wage, colour = education, data = training, geom = "density")
#the <hs grad is denser at low wage while the advanced degree is centered at higher wages

#plot for training data only - using the test will lead to overfitting

#Prepocessing predictor variables
#why preprocess - center and scale
rm(list = ls())
library(caret)
library(kernlab)
data(spam)

#create a data partition for the testing and training set
#example of data sclicing
spaminTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
spamtraining <- spam[spaminTrain, ]
spamtesting <- spam[-spaminTrain, ]
names(spamtesting[, -58])
hist(training$capitalAve, main = "", xlab = "ave. capital run length")
#skewed data is hard for model based prediction
summary(training$capitalAve)
mean(training$capitalAve) #5.681264
sd(training$capitalAve) #36.13281
#skewed and highly variable should be standardized - subtract mean and divide by sd
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(trainCapAveS) #approximately 0
sd(trainCapAveS) #exactly 1

#must use mean and sd from training to standardize the test set - mean will not be 0
#sd - will not be 1, but they should be close

#standardize with the preprocess function - center and scale
preObj <- preProcess(training[, -58], method = c("center", "scale"))
#-58 removes the variable we care about
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAveS) #approximately 0
sd(trainCapAveS) #approximately 1

#use the preprocessing from training to predict using the test set
testCapAveS <- predict(preObj, testing[, -58])$capitalAve
mean(testCapAveS) #-0.05422835
sd(testCapAveS) #0.2869551

#use the preprocess functions directly in train
set.seed(32343)
modelFit <- train(type ~ ., data = training,
                  preProcess = c("center", "scale"),
                  method = "glm")
modelFit
# Generalized Linear Model 
# 
# 3451 samples
# 57 predictor
# 2 classes: 'nonspam', 'spam' 
# 
# Pre-processing: centered (57), scaled (57) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
# Resampling results:
#         
#         Accuracy   Kappa    
# 0.9175437  0.8257259

#Preprocess method - Box-Cox transformations
#set of transformation that take continuous data and try to distribute normally
preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1, 2))
hist(trainCapAveS)
qqnorm(trainCapAveS)

#handled most issues except for repeating 0s

#preprocess to impute missing data
set.seed(13343)

#add some nas for testing
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.5) == 1
training$capAve[selectNA] <- NA

#impute and standardize - k nearest neighbor imputation- finds 'k' nearest neighbors
#and averages to impute missing data
preObj <- preProcess(training[, -58], method = "knnImpute")
capAve <- predict(preObj, training[, -58])$capAve

#standardize the true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)

quantile(capAve - capAveTruth)
#shows difference between imputed and actual values is near 0

#look at imputed values only
quantile((capAve - capAveTruth)[selectNA])
#mostly close to 0 - but more variable than using all

quantile((capAve - capAveTruth)[!selectNA])
#closer than imputed but not by much


#Covariate Creation
#covariates are variables included in model - used to combine them to predict
#the outcome of cocern.
#convert raw data into features or covariates - variables that describe the data
#as best as possible while allowing for compression for use in machine learning algos

#email example
#level 1 from raw data to covariate
#average number of capitals - capitalAve
#occurences of 'you'
#number of dollar signs - numDollar

#level 2 transforming tidy covariates - more useful variables
rm(list = ls())
library(kernlab)
data(spam)
#create a square of the covariate for us in modeling or machine learning algos
spam$capitalAveSq <- spam$capitalAve ^ 2

#new example
rm(list = ls())
library(ISLR)
library(caret)
data(Wage)
#create indexes for datasets
inTrain <- createDataPartition(y = Wage$wage,
                               p = 0.7,
                               list = FALSE)
#separate data into test and train
training <- Wage[inTrain, ]
testing <- Wage[-inTrain, ]

#turn factor variables into dummy variables (indicator variables)
table(training$jobclass)
# 1. Industrial 2. Information 
# 1068           1034
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = testing))
#               jobclass.1.Industrial jobclass.2. Information
# 161300                      1                       0
# 450601                      1                       0
# 377954                      0                       1
# 153561                      1                       0
# 447660                      1                       0
# 158226                      0                       1

#remove zeros from the covariates
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv
#             freqRatio percentUnique zeroVar   nzv
# year        1.091445    0.33301618   FALSE FALSE
# age         1.154930    2.85442436   FALSE FALSE
# sex         0.000000    0.04757374    TRUE  TRUE
# maritl      3.226667    0.23786870   FALSE FALSE
# race        8.713568    0.19029496   FALSE FALSE
# education   1.333333    0.23786870   FALSE FALSE
# region      0.000000    0.04757374    TRUE  TRUE
# jobclass    1.032882    0.09514748   FALSE FALSE
# health      2.526846    0.09514748   FALSE FALSE
# health_ins  2.263975    0.09514748   FALSE FALSE
# logwage     1.150000   19.41008563   FALSE FALSE
# wage        1.150000   19.41008563   FALSE FALSE
#throw out variables like sex and region that have no variablity and should not be
#used in a predictive model


#spline basis - fitting straight lines through the data
library(splines)
bsBasis <- bs(training$age, df = 3)
bsBasis
#bs creates a ploynomial variable - of df - so in our case three new variables
#Age - age^2 and age^3
#these allow for curvy model fitting
par(mfrow = c(1,1))
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), col = "red", pch = 19, cex = 0.5)

#splines on the test set - create the covariates on the test data set
#use the bsbasis from training to use on testing
#do not create new variables on test set - would be unrelated and potentially bias
predict(bsBasis, age = training$age)

#Covariate creation wrapup
# Level 1 feature creation (raw data to covariates)
# Science is key. Google "feature extraction for [data type]"
# Err on overcreation of features
# In some applications (images, voices) automated feature creation is possible/necessary

# Level 2 feature creation (covariates to new covariates)
# The function preProcess in caret will handle some preprocessing.
# Create new covariates if you think they will improve fit
# Use exploratory analysis on the training set for creating them
# Be careful about overfitting!

#If you want to fit spline models, use the gam method in the caret package 
#which allows smoothing of multiple variables

#Preprocessing with principal component analysis
rm(list = ls())
library(kernlab)
data(spam)
#create indexes for datasets
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
#separate data into test and train
training <- spam[inTrain, ]
testing <- spam[-inTrain, ]

#Correlated predictors
M <- abs(cor(training[,-58])) #-58 removes outcome
diag(M) <- 0
which(M > 0.8, arr.ind=T)
#        row col
# num857  32  40
# num415  34  40
#view the corresponding rows from the spam data set
names(spam)[c(32, 34)]
#"num857" "num415"

plot(spam[, 34], spam[, 32])

#idea behind pca - principal component analysis
# We might not need every predictor
# A weighted combination of predictors might be better
# We should pick this combination to capture the "most information" possible
# Benefits
# Reduced number of predictors
# Reduced noise (due to averaging)

#find combination of variables that explains the most variability

#use to rotate the plot
x <- 0.71 * training$num415 + 0.71 * training$num857
y <- 0.71 * training$num415 - 0.71 * training$num857
plot(x, y)
#shows most variability on x axis - but most are clustered on one y value
#use the sum as it reduces noise and decreases explanatory variables

#Find a new set of multivariate variables that are uncorrelated and explain as 
#much variance as possible.
#If you put all the variables together in one matrix, find the best matrix created 
#with fewer variables (lower rank) that explains the original data.

#singular value decomposition - svd
# If X is a matrix with each variable in a column and each observation in a row 
# then the SVD is a "matrix decomposition"
# 
# X=UDVT
# where the columns of U are orthogonal (left singular vectors), the columns of V 
# are orthogonal (right singluar vectors) and D is a diagonal matrix (singular values).

# PCA
# The principal components are equal to the right singular values if you first 
# scale (subtract the mean, divide by the standard deviation) the variables.

#pca in r
smallSpam <- spam[, c(32, 34)]
#prcomp for pricnicpal components
prComp <- prcomp(smallSpam)
# Standard deviations:
#         [1] 0.46482184 0.02063535
# 
# Rotation:
#         PC1        PC2
# num857 0.7061498 -0.7080625
# num415 0.7080625  0.7061498
plot(prComp$x[, 1], prComp$x[, 2])
#plot first pc by second pc
prComp$rotation
# PC1        PC2
# num857 0.7061498 -0.7080625
# num415 0.7080625  0.7061498

#PCA on Spam data
typeColor <- ((spam$type == "spam")*1 + 1)
#calculates pcs on entire dataset - used the log to make the variables more gaussian
prComp <- prcomp(log10(spam[, -58] + 1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
#pc1 includes variables which explain the most variance in the data 
#pc2 explains the set of variables which explains the second largest amount of variance

#pca with the caret package
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
#for new data what is the prediction
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)

#preprocessing with PCA
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
#create training predictions
#relates training model to principal components

# for the testing - pass the preprocess from training but with the test data
testPC <- predict(preProc,log10(testing[,-58]+1))


confusionMatrix(testing$type,predict(modelFit,testPC))
# Confusion Matrix and Statistics
# 
# Reference
# Prediction nonspam spam
# nonspam     638   59
# spam         63  390
# 
# Accuracy : 0.8939          
# 95% CI : (0.8747, 0.9111)
# No Information Rate : 0.6096          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.7775          
# Mcnemar's Test P-Value : 0.7859          
# 
# Sensitivity : 0.9101          
# Specificity : 0.8686          
# Pos Pred Value : 0.9154          
# Neg Pred Value : 0.8609          
# Prevalence : 0.6096          
# Detection Rate : 0.5548          
# Detection Prevalence : 0.6061          
# Balanced Accuracy : 0.8894          
# 
# 'Positive' Class : nonspam

#embed preprocess into training
modelFit <- train(training$type ~ .,method="glm",preProcess="pca",data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

#final thoughts on PCA
# Most useful for linear-type models
# Can make it harder to interpret predictors
# Watch out for outliers!
#       Transform first (with logs/Box Cox)
#       Plot predictors to identify problems