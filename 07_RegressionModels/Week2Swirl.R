rm(list = ls())
library(swirl)
swirl()
Brian
6
3
4
#residuals
#the outcome (Y) with the linear association of the predictor (X) removed.
fit <- lm(child ~ parent, data = galton)

#the following three commands all calculate the standard deviation of the residuals
sqrt(sum(fit$residuals^2)/(n-2))
summary(fit)$sigma
sqrt(deviance(fit)/(n-2))

#y - mean(y) - represents the total variation
#y - y_hat - represents the residual variation - vertical distance between actual 
#values and estimates(yhats)

#store the mean
mu <- mean(galton$child)
#store the sum of squares - center the data
sTot <- sum((galton$child - mu)^2)
#use deviance to store sum of squares of residuals
sRes <- deviance(fit)

#calculate R^2 1 - sRes/sTot
1 - sRes/sTot
summary(fit)$r.squared

cor(galton)^2
cor(galton$parent, galton$child)^2
2
brian_linn@calquake.com
fnZWIiTljaDCEDm6

#residual variation
fit <- lm(child ~ parent, data = galton)
sqrt(sum(fit$residuals^2) / (n - 2))
summary(fit)$sigma
sqrt(deviance(fit)/(n-2))
1
3
3
mu <- mean(galton$child)
sTot <- sum((galton$child - mu)^2)
sRes <- deviance(fit)
1 - sRes/sTot
summary(fit)$r.squared
cor(galton$child, galton$parent)^2
2
brian_linn@calquake.com

#multivariable regression
library(swirl)
swirl()
Brian
6
3
5

#store '1's - as many as there are rows in galton
ones <- rep(1, nrow(galton))
#now subract 1 from the predictor to exclude the default intercept
#suppresses the intercept
lm(child ~ ones + parent - 1, data = galton)
#Call:
#        lm(formula = child ~ ones + parent - 1, data = galton)
#Coefficients:
#ones   parent  
#23.9415   0.6463 
lm(child ~ parent, galton)
#Call:
#        lm(formula = child ~ parent, data = galton)
#Coefficients:
#(Intercept)       parent  
#23.9415       0.6463
#this demonstrates that the default regression includes 1 as the intercept for each value?
#The regression in one variable given by lm(child ~ parent, galton) really involves two regressors,
#| the variable, parent, and a regressor of all ones.


#by subtracting the means, we eliminate one of the two regressors, the constant, leaving
#just one, parent. The coefficient of the remaining regressor is the slope.
#Subtracting the means to eliminate the intercept is a special case of a general technique which is
#sometimes called Gaussian Elimination. As it applies here, the general technique is to pick one
#regressor and to replace all other variables by the residuals of their regressions against that
#one

#The mean of a variable is the coefficient of its regression against the constant, 1. Thus,
#subtracting the mean is equivalent to replacing a variable by the residual of its regression
#against 1.

lm(child ~ 1, galton)
#Coefficients:
#(Intercept)  
#68.09 is the same as the mean height for children

#predict the Volume of timber which a tree might produce from measurements of 
#its Height and Girth.

head(trees)
#create an lm with three regressors
fit <- lm(Volume ~ Girth + Height + Constant - 1, data = trees)
#now remove the Girth regressor with eliminate
trees2 <- eliminate("Girth", trees)
head(trees2)
3
#the Constant column is not constant?  because The constant, 1, has been 
#replaced by its residual when regressed against Girth.
#create model with reduced data set
fit2 <- lm(Volume ~ Height + Constant - 1, data = trees2)

#print coefficients of fit and fit2 for comparison.
lapply(list(fit, fit2), coef)
[[1]]
Girth      Height    Constant 
4.7081605   0.3392512 -57.9876589 

[[2]]
Height    Constant 
0.3392512 -57.9876589 

1
#Suppose we were given a multivariable regression problem involving an outcome and N regressors,
#| where N > 1. Using only single-variable regression, how can the problem be reduced to a problem
#| with only N-1 regressors?

#Pick any regressor and replace the outcome and all other regressors by their 
#residuals against the chosen one.

#multivariable regression example
rm(list = ls())
library(swirl)
swirl()
Brian
6
3
6
makelms <- function(){
        # Store the coefficient of linear models with different independent variables
        cf <- c(coef(lm(Fertility ~ Agriculture, swiss))[2], 
                coef(lm(Fertility ~ Agriculture + Catholic,swiss))[2],
                coef(lm(Fertility ~ Agriculture + Catholic + Education,swiss))[2],
                coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination,swiss))[2],
                coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination +Infant.Mortality, swiss))[2])
        print(cf)
}

# Regressor generation process 1.
rgp1 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducability
        set.seed(4321)
        # Point A:
        x1 <- rnorm(n)
        x2 <- rnorm(n)
        x3 <- rnorm(n)
        # Point B:
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
        round(apply(betas, 1, var), 5)
}

# Regressor generation process 2.
rgp2 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducability
        set.seed(4321)
        # Point C:
        x1 <- rnorm(n)
        x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
        x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
        # Point D:
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
        round(apply(betas, 1, var), 5)
}


#start here
#the '.' represents all the five columns in 'swiss'
all <- lm(Fertility ~ ., data = swiss)
summary(all)$coef
#expect a 0.17 decrease in standardized fertility for every 1% increase in the 
#percentage of males involved in agriculture

#now fertility with agriculture only
summary(lm(Fertility ~ Agriculture, data = swiss))$coef
1
1
cor(swiss$Examination, swiss$Education)
#.698 - more education correlated with higher exam scores
cor(swiss$Agriculture, swiss$Education)
#-.639 more education leads to less agriculture and vice versa

makelms()
#Agriculture Agriculture Agriculture Agriculture Agriculture 
#0.1942017   0.1095281  -0.2030377  -0.2206455  -0.1721140 
#shows that Education changes the coefficient to negative

ec <- swiss$Examination + swiss $Catholic
efit <- lm(Fertility ~ . + ec, data = swiss)
all$coefficients - efit$coefficients
2
1
1
brian_linn@calquake.com
xbCVtt6w4gM72gVD
