#Regularized Regression
# Basic idea
#       Fit a regression model
#       Penalize (or shrink) large coefficients
# Pros:
#       Can help with the bias/variance tradeoff
#       Can help with model selection
# Cons:
#       May be computationally demanding on large data sets
#       Does not perform as well as random forests and boosting

#Prostate Cancer Example
remove(list = ls())
library(ElemStatLearn)
data("prostate")
str(prostate)

# Model selection approach: split samples
# No method better when data/computation time permits it
# 
# Approach
# 
# Divide data into training/test/validation
# Treat validation as test data, train all competing models on the train data 
# and pick the best one on validation.
# To appropriately assess performance on new data apply to test set
# You may re-split and reperform steps 1-3
# Two common problems
#       Limited data
#       Computational complexity

#Issue for high-dimensional data
small <- prostate[1:5, ]
lm(lpsa ~ ., data = small)
# Call:
#         lm(formula = lpsa ~ ., data = small)
# 
# Coefficients:
# (Intercept)       lcavol      lweight          age         lbph          svi          lcp  
# 9.60615      0.13901     -0.79142      0.09516           NA           NA           NA  
# gleason        pgg45    trainTRUE  
# -2.08710           NA           NA 
# not all predictors received estimates as there were not enough samples

# Regularization for regression
# If the ??j's are unconstrained:
#       They can explode
#       And hence are susceptible to very high variance
 
# To control variance, we might regularize/shrink the coefficients.
#       PRSS(??)=???j=1n(Yj??????i=1m??1iXij)2+P(??;??)
# where PRSS is a penalized form of the sum of squares. Things that are commonly looked for
#       Penalty reduces complexity
#       Penalty reduces variance
#       Penalty respects structure of the problem

# Ridge regression - penalized regression
# Solve:
#         
#         ???i=1N??????yi?????0+???j=1pxij??j??????2+?????j=1p??2j
# equivalent to solving
# 
# ???Ni=1(yi?????0+???pj=1xij??j)2 subject to ???pj=1??2j???s where s is inversely proportional to ??
# 
# Inclusion of ?? makes the problem non-singular even if XTX is not invertible.

# Tuning parameter ??
# ?? controls the size of the coefficients
# ?? controls the amount of {\bf regularization}
# As ?????0 we obtain the least square solution
# As ???????? we have ??^ridge??=???=0

# Lasso - penalized regression
# ???Ni=1(yi?????0+???pj=1xij??j)^2 subject to ???pj=1|??j|???s
# also has a lagrangian form
# ???i=1N(??????yi?????0+???j=1pxij??j)^??????2+?????j=1p|??j|
# For orthonormal design matrices (not the norm!) this has a closed form solution
# ??^j=sign(??^0j)(|??^0j?????)+
# but not in general.

#In caret the methods are - ridge, lasso, relaxo

###############################################################################
###############################################################################
#Combining Predictors - ensembling methods
# Key ideas
# You can combine classifiers by averaging/voting
# Combining classifiers improves accuracy
# Combining classifiers reduces interpretability
# Boosting, bagging, and random forests are variants on this theme

# Basic intuition - majority vote
# Suppose we have 5 completely independent classifiers
# 
# If accuracy is 70% for each:
#       10 × (0.7)^3 x (0.3)^2 + 5 × (0.7)^4 x (0.3)^2 + (0.7)^5
#       83.7% majority vote accuracy
# With 101 independent classifiers
#       99.9% majority vote accuracy

# Approaches for combining classifiers
# Bagging, boosting, random forests
#       Usually combine similar classifiers
# Combining different classifiers
#       Model stacking
#       Model ensembling

# Model Stacking Example with Wage Data
remove(list = ls())
library(ISLR)
data("Wage")
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage))

#Create building and validation sets
inBuild <- createDataPartition(y = Wage$wage,
                               p = 0.7,
                               list = FALSE)
validation <- Wage[-inBuild, ]
buildData <- Wage[inBuild, ]

#Create training and testing data
inTrain <- createDataPartition(y = buildData$wage,
                               p = 0.7,
                               list = FALSE)

training <- buildData[inTrain, ]
testing <- buildData[-inTrain, ]
dim(validation)# 898 11
dim(buildData)# 2102 11
dim(training)# 1474 11
dim(testing)# 628 11

#Build the models with glm and rf methods
mod1 <- train(wage ~ ., method = "glm", data = training)
mod2 <- train(wage ~ ., method = "rf", data = training,
              trcontrol = trainControl(method = "cv"), number = 3)

#Predict on the testing set
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)

#plot the predictions
qplot(pred1, pred2, colour = wage, data = testing)

#combine the predictors into one model
predDF <- data.frame(pred1, pred2, wage = testing$wage)
combModFit <- train(wage ~ ., method = "gam", data = predDF)
combPred <- predict(combModFit, predDF)

#testing errors
#prediction 1 based off of the general linear model
sqrt(sum((pred1 - testing$wage)^2)) #837.5226
#prediction 2 based off of the random forest model
sqrt(sum((pred2 - testing$wage)^2)) #852.4964
#combined prediction based off of the glm + rf models
sqrt(sum((combPred - testing$wage)^2)) #818.8396
#the combined predictor has the lowest error rate

#now predict using the validation data set
pred1V <- predict(mod1, validation)
pred2V <- predict(mod2, validation)
#combine the two predictions
predVDF <- data.frame(pred1 = pred1V, pred2 = pred2V)
#predict using the combined predictions
combPredV <- predict(combModFit, predVDF)

#validation errors
#prediction 1 based off of the general linear model
sqrt(sum((pred1V - validation$wage)^2)) #1046.807
#prediction 2 based off of the random forest model
sqrt(sum((pred2V - validation$wage)^2)) #1071.275
#combined prediction based off of the glm + rf models
sqrt(sum((combPredV - validation$wage)^2)) #1047.148
#the combined predictor is not as good as the linear model alone for the 
#validation data - potentially due to overfitting

###############################################################################
###############################################################################
#Forecasting

#Time-series data
# What is different?
        # Data are dependent over time
# Specific pattern types
#       Trends - long term increase or decrease
#       Seasonal patterns - patterns related to time of week, month, year, etc.
#       Cycles - patterns that rise and fall periodically
# Subsampling into training/test is more complicated
# Similar issues arise in spatial data
#       Dependency between nearby observations
#       Location specific effects
# Typically goal is to predict one or more observations into the future.
# All standard predictions can be used (with caution!)

#caution with potentially spurious correlations 
#       e.g. web searches for 'solitaire network' and goog price
#beware of extrapolation

#Example with Google data
remove(list = ls())
library(quantmod)
from.dat <- as.Date("01/01/08", format = "%m/%d/%y")
to.dat <- as.Date("12/31/2013", format = "%m/%d/%y")
getSymbols("GOOG", src = "google", from = from.dat, to = to.dat)
head(GOOG[GOOG$GOOG.Volume > 0])

#summarize monthly and store as time series
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency = 12)
plot(ts1, xlab = "Years + 1", ylab = "GOOG")

# Example time series decomposition
#       Trend - Consistently increasing pattern over time
#       Seasonal - When there is a pattern over a fixed period of time that recurs.
#       Cyclic - When data rises and falls over non fixed periods

#decompose the time series into its parts
plot(decompose(ts1), xlab = "Years + 1")

#train and test sets
ts1Train <- window(ts1, start = 1, end = 5)
ts1test <- window(ts1, start = 5, end = (7-0.01))

#simple moving average
plot(ts1Train)
library(forecast)
lines(ma(ts1Train, order = 3), col = "red")
#exponential smoothing
ets1 <- ets(ts1Train, model = "MMM")
fcast <- forecast(ets1)
plot(fcast)
lines(ts1Train, col = "red")

#get the accuracy
accuracy(fcast, ts1Train)

###############################################################################
###############################################################################
#Unsupervised Forecasting
# Key ideas
# Sometimes you don't know the labels for prediction
# To build a predictor
#       Create clusters
#       Name clusters
#       Build predictor for clusters
# In a new data set
#       Predict clusters

#Iris example - ignoring species
data(iris)
library(ggplot2)
library(caret)
intrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[intrain, ]
testing <- iris[-intrain, ]
dim(training) #105 5
dim(testing) #45 5

#cluster with k-means
kMeans1 <- kmeans(subset(training, select = -c(Species)), centers = 3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour = clusters, data = training)

#Compare to the real labels
table(kMeans1$cluster, training$Species)
# setosa versicolor virginica
# 1     35          0         0
# 2      0         34        11
# 3      0          1        24

#build the predictor
library(rpart)
#classification tree - rpart
modFit <- train(clusters ~ ., data = subset(training, select = -c(Species)), 
                method = "rpart")
table(predict(modFit, training), training$Species)
# setosa versicolor virginica
# 1     35          0         0
# 2      0         35        13
# 3      0          0        22

#Apply to the test data set
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)
# testClusterPred setosa versicolor virginica
# 1     15          0         0
# 2      0         15         3
# 3      0          0        12