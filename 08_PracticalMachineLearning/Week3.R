#Week3
#Predicting with Trees

# Basic algorithm
# Start with all variables in one group
# Find the variable/split that best separates the outcomes
# Divide the data into two groups ("leaves") on that split ("node")
# Within each split, find the best variable/split that separates the outcomes
# Continue until the groups are too small or sufficiently "pure"

#miscalssification
#Gini
#informaiton

#Example with Iris data
remove(list = ls())
data(iris)
library(ggplot2)
library(caret)
names(iris)
#"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"  
table(iris$Species)
#setosa versicolor  virginica 
#50         50         50 

#create the training and test sets
inTrain <- createDataPartition(y= iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

dim(training) #105 5
dim(testing) #45 5

#exploratory plot
qplot(Petal.Width, Sepal.Width, data = training, colour = Species)

#comparing petal and sepal width - create tree with rpart
modFit <- train(Species ~ ., method = "rpart", data = training)
print(modFit$finalModel)
# n= 105 
# 
# node), split, n, loss, yval, (yprob)
# * denotes terminal node
# 
# 1) root 105 70 setosa (0.33333333 0.33333333 0.33333333)  
# 2) Petal.Length< 2.6 35  0 setosa (1.00000000 0.00000000 0.00000000) *
#         3) Petal.Length>=2.6 70 35 versicolor (0.00000000 0.50000000 0.50000000)  
# 6) Petal.Length< 4.75 32  0 versicolor (0.00000000 1.00000000 0.00000000) *
#         7) Petal.Length>=4.75 38  3 virginica (0.00000000 0.07894737 0.92105263) *
#2 indicates that all petal length below 2.6 will be setosa
#Plot the tree
plot(modFit$finalModel, uniform = TRUE,
     main = "Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex = 0.8)
#better looking version of the tree

library(rattle)
fancyRpartPlot(modFit$finalModel)

#use the model to predict new values
predict(modFit, newdata = testing)
# [1] setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa    
# [11] setosa     setosa     setosa     setosa     setosa     versicolor versicolor versicolor versicolor versicolor
# [21] virginica  virginica  versicolor virginica  versicolor versicolor versicolor versicolor versicolor versicolor
# [31] virginica  virginica  virginica  virginica  versicolor virginica  virginica  virginica  virginica  virginica 
# [41] virginica  virginica  virginica  virginica  virginica 
# Levels: setosa versicolor virginica

###############################################################################
###############################################################################
#Bagging or bootstrap aggregating
#idea that the average of many complicated models may be a smoother fit with 
#better balance between variance and potential bias
# Basic idea:
# Resample cases and recalculate predictions
# Average or majority vote
# 
# Notes:
# Similar bias
# Reduced variance
# More useful for non-linear functions

#Example with Ozone data
remove(list = ls())

library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
head(ozone)
# ozone radiation temperature wind
# 1    41       190          67  7.4
# 2    36       118          72  8.0
# 3    12       149          74 12.6
# 4    18       313          62 11.5
# 5    23       299          65  8.6
# 6    19        99          59 13.8

#Bagged loess - bootsrap aggregating local regression
ll <- matrix(NA, nrow = 10, ncol = 155)
#sample the data set 10 times
for(i in 1:10) {
        ss <- sample(1:dim(ozone)[1], replace = T)
        ozone0 <- ozone[ss, ]
        ozone0 <- ozone0[order(ozone0$ozone), ]
        #fit the loess curve to each sample
        loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
        #predict for loess curve the values for ozone
        ll[i, ] <- predict(loess0, newdata = data.frame(ozone = 1: 155))
        
}
#plot the predictions
plot(ozone$ozone, ozone$temperature, pch = 19, cex = 0.5)
#plot all of the fitted lines
for(i in 1:10){
        lines(1:155, ll[i, ], col = "grey", lwd = 2)
}
#plot the mean of the fitted lines for each sample data set
lines(1:155, apply(ll, 2, mean), col = "red", lwd = 2) #2 for columns

# Some models perform bagging for you, in train function consider method options
# bagEarth
# treebag
# bagFDA
# Alternatively you can bag any model you choose using the bag function

#more bagging in caret
predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

#custom bagging continued
plot(ozone$ozone, temperature, col = 'lightgrey', pch = 19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), pch = 19, col = "red")
points(ozone$ozone, predict(treebag, predictors), pch = 19, col = "blue")
#grey dots are observed values
# the blue or red dots represent the fit from a single conditional regression tree

#parts of bagging
ctreeBag$fit #train a conditional regression tree
function (x, y, ...) 
{
        loadNamespace("party")
        data <- as.data.frame(x)
        data$y <- y
        party::ctree(y ~ ., data = data)
}
<environment: namespace:caret>
        
ctreeBag$pred
{
        if (!is.data.frame(x)) 
                x <- as.data.frame(x)
        obsLevels <- levels(object@data@get("response")[, 1])
        if (!is.null(obsLevels)) {
                rawProbs <- party::treeresponse(object, x)
                probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
                                     byrow = TRUE)
                out <- data.frame(probMatrix)
                colnames(out) <- obsLevels
                rownames(out) <- NULL
        }
        else out <- unlist(party::treeresponse(object, x))
        out
}
<environment: namespace:caret>
        
ctreeBag$aggregate #aggregation takes predictions from all model fits
function (x, type = "class") 
{
        if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
                pooled <- x[[1]] & NA
                classes <- colnames(pooled)
                for (i in 1:ncol(pooled)) {
                        tmp <- lapply(x, function(y, col) y[, col], col = i)
                        tmp <- do.call("rbind", tmp)
                        pooled[, i] <- apply(tmp, 2, median)
                }
                if (type == "class") {
                        out <- factor(classes[apply(pooled, 1, which.max)], 
                                      levels = classes)
                }
                else out <- as.data.frame(pooled)
        }
        else {
                x <- matrix(unlist(x), ncol = length(x))
                out <- apply(x, 1, median)
        }
        out
}
<environment: namespace:caret>
        
###############################################################################
###############################################################################
#Random Forests
# Bootstrap samples
# At each split, bootstrap variables
# Grow multiple trees and vote

        # Pros:
#       Accuracy
# Cons:
#       Speed
#       Interpretability
#       Overfitting

#sample with iris data
remove(list = ls())
data(iris)
library(ggplot2)
library(caret)
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]

modFit <- train(Species ~ ., data = training, method = "rf", prox = TRUE)
modFit
# Random Forest 
# 
# 105 samples
# 4 predictor
# 3 classes: 'setosa', 'versicolor', 'virginica' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 105, 105, 105, 105, 105, 105, ... 
# Resampling results across tuning parameters:
#         
#  mtry  Accuracy   Kappa    
# 2     0.9312419  0.8954149
# 3     0.9308351  0.8947719
# 4     0.9288522  0.8917707
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was mtry = 2. 

#looking at a single tree
getTree(modFit$finalModel, k = 2)
# left daughter right daughter split var split point status prediction
# 1             2              3         3        2.60      1          0
# 2             0              0         0        0.00     -1          1
# 3             4              5         4        1.75      1          0
# 4             6              7         4        1.45      1          0
# 5             0              0         0        0.00     -1          3
# 6             0              0         0        0.00     -1          2
# 7             8              9         2        2.90      1          0
# 8             0              0         0        0.00     -1          3
# 9             0              0         0        0.00     -1          2


#class 'centers'
irisP <- classCenter(training[, c(3, 4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col = Species, data = training)
p + geom_point(aes(x = Petal.Width, y = Petal.Length, col = Species), size = 5,
               shape = 4, data = irisP)

#predicting new values
pred <- predict(modFit, testing)
testing$predRight <- pred==testing$Species
table(pred, testing$Species)
# pred         setosa versicolor virginica
# setosa         15          0         0
# versicolor      0         14         0
# virginica       0          1        15


#plot the new predictions
qplot(Petal.Width, Petal.Length, colour = predRight, data = testing, main = "newdata Predictions")


###############################################################################
###############################################################################
#Boosting
# Basic idea
# Take lots of (possibly) weak predictors
# Weight them and add them up
# Get a stronger predictor

# Start with a set of classifiers h1,.,hk
# Examples: All possible trees, all possible regression models, all possible cutoffs.
# Create a classifier that combines classification functions: f(x)=sgn(???Tt=1??tht(x)).
# Goal is to minimize error (on training set)
# Iterative, select one h at each step
# Calculate weights based on errors
# Upweight missed classifications and select next h

# Boosting can be used with any subset of classifiers
# One large subclass is gradient boosting
# R has multiple boosting libraries. Differences include the choice of basic classification functions and combination rules.
# gbm - boosting with trees.
# mboost - model based boosting
# ada - statistical boosting based on additive logistic regression
# gamBoost for boosting generalized additive models
# Most of these are available in the caret package

#Wage Example
rm(list = ls())
library(ISLR); 
data(Wage); 
library(ggplot2); 
library(caret);
Wage <- subset(Wage,select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; 
testing <- Wage[-inTrain,]

modFit <- train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)
# Stochastic Gradient Boosting 
# 
# 2102 samples
# 10 predictor
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 2102, 2102, 2102, 2102, 2102, 2102, ... 
# Resampling results across tuning parameters:
#         
# interaction.depth  n.trees  RMSE      Rsquared 
# 1                   50      34.26223  0.3117397
# 1                  100      33.73576  0.3202348
# 1                  150      33.64713  0.3222129
# 2                   50      33.69567  0.3235551
# 2                  100      33.49873  0.3278147
# 2                  150      33.51642  0.3271154
# 3                   50      33.58224  0.3260793
# 3                  100      33.62155  0.3234369
# 3                  150      33.74582  0.3195681
# 
# Tuning parameter 'shrinkage' was held constant at a value of 0.1
# Tuning parameter 'n.minobsinnode' was
# held constant at a value of 10
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were n.trees = 100, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode
# = 10. 

#plot the results of the model predictions with the testing set
qplot(predict(modFit, testing), wage, data = testing)

###############################################################################
###############################################################################
#Model Based Prediction
# Basic idea
# Assume the data follow a probabilistic model
# Use Bayes' theorem to identify optimal classifiers
# Pros:
# Can take advantage of structure of the data
# May be computationally convenient
# Are reasonably accurate on real problems

# Cons:
# Make additional assumptions about the data
# When the model is incorrect you may get reduced accuracy

# Model based approach
# Our goal is to build parametric model for conditional distribution P(Y=k|X=x)
# A typical approach is to apply Bayes theorem:
#         Pr(Y=k|X=x)= Pr(X=x|Y=k)Pr(Y=k) / ???K???=1Pr(X=x|Y=???)Pr(Y=???)
# Pr(Y=k|X=x)=fk(x)??k???K???=1f???(x)?????
# Typically prior probabilities ??k are set in advance.
# 
# A common choice for fk(x)=1??k2?????e???(x?????k)2??2k, a Gaussian distribution
# 
# Estimate the parameters (??k,??2k) from the data.
# 
# Classify to the class with the highest value of P(Y=k|X=x)

# Classifying using the model
# A range of models use this approach
# 
# Linear discriminant analysis assumes fk(x) is multivariate Gaussian with same covariances
# Quadratic discrimant analysis assumes fk(x) is multivariate Gaussian with different covariances
# Model based prediction assumes more complicated versions for the covariance matrix
# Naive Bayes assumes independence between features for model building

# Discriminant function
# ??k(x)=xT?????1??k???12??k?????1??k+log(??k)
# Decide on class based on Y^(x)=argmaxk??k(x)
# We usually estimate parameters with maximum likelihood

#Example with Iris data
rm(list = ls())
data(iris)
library(ggplot2)
names(iris) 
#"Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"  
table(iris$Species)
#setosa versicolor  virginica 
#50         50         50 

#create the training and testing data sets
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training)
dim(testing)

#build the model and predictions
modlda <- train(Species ~ ., data = training, method = "lda")
modnb <-   train(Species ~ ., data = training, method = "nb")

plda <- predict(modlda, testing)
pnb <- predict(modnb, testing)
table(plda, pnb)
# 
#               pnb
# plda         setosa versicolor virginica
# setosa         15          0         0
# versicolor      0         15         1
# virginica       0          1        13

#compare the results
equalprediction <- (plda==pnb)
qplot(Petal.Width, Sepal.Width, colour = equalprediction, data = testing)
