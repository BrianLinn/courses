#Using regression for prediction
rm(list = ls())
library(UsingR)
library(ggplot2)
require(UsingR)
require(ggplot2)
data(diamond)
#plot the data with ggplot
g <- ggplot(diamond, aes(x = carat, y = price))
g <- g + xlab("Mass (carats)")
g <- g + ylab("Price (SIN $)")
g <- g + geom_point(size = 7, color = "black", alpha = 0.5)
g <- g + geom_point(size = 5, color = "blue", alpha = 0.2)
g <- g + geom_smooth(method = "lm", color = "black")
g
#lm(Y ~ X) - where Y = response, X = predictor
#lm(response ~ predictor)
#Fit the line to the model
fit <- lm(price ~ carat, data = diamond)
fit$coefficients
coef(fit)
#intercept = -259.62, and carat = 3721.0249
#so ,there is a 3721$ increase in price per carat increase
#at 0 carats, the price would be -259.62

#0 is not a good intercept as there are no 0 carat diamonds
#now fit again, but this time center the X variable
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
fit2$coefficients
#(Intercept) I(carat - mean(carat)) 
#500.0833              3721.0249
mean(diamond$carat)
#0.2041667
#this means that for the average sized diamond (0.2041667 grams about 1 carat) 
# should have a price of 500.08, #the slope was unchanged

#now we will run the same centered model, but this time we will scale it, so that
# we can judge a 1/10th carat increase, since 1 carat jumps are large
coef(fit2)/10
#(Intercept) I(carat - mean(carat)) 
#50.00833              372.10249 
#this means we expect a 372.10 increase in price for each 1/10 carat increase

#now use R to show the same thing as dividing by 10 in a refitted model
fit3 <- lm(price ~ I(carat * 10), data = diamond)
fit3$coefficients
#(Intercept) I(carat * 10) 
#-259.6259      372.1025 
#now we expect a 372.10 increase in each 1/10th per carat increase 0 theame as above

#predict prices for the following carat sizes
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit2)[2] * newx
#intercept from the first model -259.62 - the price at 0 + 
#the coeffecient(slope) from the centered model, 3721.02 * the carat size(x)
#335.7381  745.0508 1005.5225
#represent the three prices respectively

#more succinctly with predict
predict(fit, newdata = data.frame(carat = newx))

#1         2         3 
#335.7381  745.0508 1005.5225

#plotting the data
data(diamond)
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
points(diamond$carat, predict(fit), pch = 19, col = "red")
lines(c(0.16, 0.16, 0.12), 
      c(200, coef(fit)[1] + coef(fit)[2] * 0.16,
        coef(fit)[1] + coef(fit)[2] * 0.16))
lines(c(0.27, 0.27, 0.12), 
      c(200, coef(fit)[1] + coef(fit)[2] * 0.27,
        coef(fit)[1] + coef(fit)[2] * 0.27))
lines(c(0.34, 0.34, 0.12), 
      c(200, coef(fit)[1] + coef(fit)[2] * 0.34,
        coef(fit)[1] + coef(fit)[2] * 0.34))
text(newx, rep(250, 3), labels = newx, pos = 2)

#1
rm(list = ls())
data(father.son)
 #response sheight
 #predictor fheight
fit <- lm(sheight ~ fheight, data = father.son)
fit$coefficients
#slope = .514093
#int = 33.886604
summary(fit)$coef
#Estimate Std. Error  t value     Pr(>|t|)
#(Intercept) 33.886604 1.83235382 18.49348 1.604044e-66
#x            0.514093 0.02704874 19.00618 1.121268e-69
#the p values are small so reject null hypothesis

#2
fit$coefficients
33.89+.514
fit2 <- lm(y ~ I(x - mean(x)))
fit2$coefficients

#3 - predict son height at father height of 80
fh <- 80
fit <- lm(sheight ~ fheight, data = father.son)
coef(fit)[1] + coef(fit2)[2] * fh
predict(fit, newdata = data.frame(fheight = 80))

#4
data(mtcars)
names(mtcars)
fit <- lm(mpg ~ hp, data = mtcars)
fit$coefficients
#center the data
fit2 <- lm(mpg ~I(hp - mean(hp)), data = mtcars)
fit2$coefficients

#overlay the fit above onto a scatterplot
plot(x = mtcars$hp, y = mtcars$mpg)
abline(fit, lwd = 2)
summary(fit)$coef

#7
#predict mpg for hp = 111
coef(fit)[1] + coef(fit2)[2] * 111
predict(fit, newdata = data.frame(hp = 111))


data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e -(y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))

#the red lines in the plot represent the residuals
plot(diamond$carat, diamond$price,  
     xlab = "Mass (carats)", 
     ylab = "Price (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(fit, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red" , lwd = 2)




plot(x, resid(lm(y ~ x))); 
abline(h = 0)


#residuals example
rm(list = ls())
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
#fit the linear model with price predicted by carat
fit <- lm(y ~ x)
#easy r supported method to calculate residuals
e <- resid(fit)
e
#to calculate manually - start with predicted Y's
yhat <- predict(fit)
#residuals are y - yhat
max(abs(e - (y - yhat)))
#check r against calculated values - max value is e-13, so 0 for this test

#now check again but hardcode in the yhat values
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
#same as above calculation - near 0

#residuals versus X
plot(diamond$carat, e,  
     xlab = "Mass (carats)", 
     ylab = "Residuals (SIN $)", 
     bg = "lightblue", 
     col = "black", cex = 1.1, pch = 21,frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1 : n) 
        lines(c(x[i], x[i]), c(e[i], 0), col = "red" , lwd = 2)

#non-linear data
x <- runif(100, -3, 3); y <- x + sin(x) + rnorm(100, sd = .2); 
plot(x, y); abline(lm(y ~ x))

#plots x against the residuals
plot(x, resid(lm(y ~ x))); 
abline(h = 0)

#heteroskedasticity
x <- runif(100, 0, 6); 
y <- x + rnorm(100,  mean = 0, sd = .001 * x); 
plot(x, y); 
abline(lm(y ~ x))


#finding residual variance estimates
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
##estimates from lm
summary(fit)$sigma
#[1] 31.84052

##calculate the residual directly
sqrt(sum(resid(fit)^2) / (n - 2))
#[1] 31.84052
#same as using the summary of the model

#shows variation explained by a model with an intercept only
e <- c(resid(lm(price ~ 1, data = diamond)),
       resid(lm(price ~ carat, data = diamond)))
e
fit <- factor(c(rep("Itc", nrow(diamond)), 
                rep("Itc, slope", nrow(diamond))))
g <- ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g <- g + geom_dotplot(binaxis = "y", dotsize = 2, stackdir = "center", binwidth = 20)
g <- g + xlab("Fiting approach")
g <- g + ylab("Residual price")
g

data(anscombe)
example(anscombe)

#1
data(father.son)
y <- father.son$sheight
x <- father.son$fheight
fit <- lm(y ~ x)
par(mfrow = c(1, 2))
plot(y, fit$residuals)
plot(y, resid(fit))
plot(fit)

#2
rm(list = ls())
data(father.son)
y <- father.son$sheight
x <- father.son$fheight
n <- length(y)
fit <- lm(y ~ x)

#manually calculate the residual variance
sum(resid(fit)^2) / (n - 2)
#[1] 5.936804
#pulling the sigma from the summary of the linear model will return the same value
summary(fit)$sigma^2
#[1] 5.936804

#3
rm(list = ls())
data(father.son)
y <- father.son$sheight
x <- father.son$fheight
n <- length(y)
fit <- lm(y ~ x)

#r squared
cor(y, x)^2
#[1] 0.2513401
#or summary
summary(fit)
#Multiple R-squared:  0.2513
summary(fit)$r.squared

#4
rm(list = ls())
data(mtcars)
fit <- lm(mpg ~ hp, data = mtcars)
plot(mtcars$hp, resid(fit))
abline(fit)

#5
sum(resid(fit)^2) / (nrow(mtcars) - 2)
#[1] 14.92248
summary(fit)$sigma^2
#[1] 14.92248

#6 r squared
summary(fit)$r.squared
#[1] 0.6024373
cor(mtcars$mpg, mtcars$hp)^2
#[1] 0.6024373


#regression inference
library(UsingR)
data(diamond)
y <- diamond$price
x <- diamond$carat
n <- length(y)
#slope
beta1 <- cor(y, x) * sd(y) / sd(x)
#intercept
beta0 <- mean(y) - beta1 * mean(x)
#errors
e <- y - beta0 - beta1 * x
#sigma - estimate of standard deviation around the regression line
#use square root to make it a standard deviation instead of variance
sigma <- sqrt(sum(e^2) / (n-2))
#sums of squares of xs
ssx <- sum((x - mean(x))^2)
#standard error for beta0
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma
seBeta1 <- sigma / sqrt(ssx)
#create the two t statistics - divide the estimate by the standard error     
#do not have to subtract off true value since it is assumed to be 0
tBeta0 <- beta0/seBeta0
tBeta1 <- beta1/seBeta1
#now calculate the p-values - 2 times the t-probability, of this statistic or larger
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
#manually generate the coefficient table
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
#fit the linear model to the data
fit <- lm(y ~ x)
#programmatically generate the coefficient table with the same results
summary(fit)$coef


#Confidence intervals
#using the fit model from diamond above
sumCoef <- summary(fit)$coefficients
#first the intercept
sumCoef[1, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[1,2]
#[1] -294.4870 -224.7649
#now the slope
(sumCoef[2, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[2,2])/10
#[1] 355.6398 388.5651
#interpreted as: with 95% confidence we estiamte that 0.1 carat increase in diamond size
#results in 355.6 to 388.6 increase in $price

#prediction demo
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"
#graphing the ci in ggplot2
g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g
#graphing the ci in base
plot(x, y, frame=FALSE,xlab="Carat",ylab="Dollars",pch=21,col="black", bg="lightblue", cex=2)
abline(fit, lwd = 2)
xVals <- seq(min(x), max(x), by = .01)
yVals <- beta0 + beta1 * xVals
se1 <- sigma * sqrt(1 / n + (xVals - mean(x))^2/ssx)
se2 <- sigma * sqrt(1 + 1 / n + (xVals - mean(x))^2/ssx)
lines(xVals, yVals + 2 * se1)
lines(xVals, yVals - 2 * se1)
lines(xVals, yVals + 2 * se2)
lines(xVals, yVals - 2 * se2)


#exercises
#1 - 
rm(list = ls())
data(father.son)
fit <- lm(sheight ~ fheight, data = father.son)
summary(fit)$coef
#now the confidence intervals
sumCoef <- summary(fit)$coefficients

sumCoef[1, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[1,2]
#[1] -294.4870 -224.7649

#now the slope
sumCoef[2, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[2,2]
#or
confint(fit)

fitC <- lm(sheight ~ I(fheight - mean(fheight)), data = father.son)
confint(fitC)
m <- mean(father.son$fheight)
#mean value prediction interval
predict(fitC, newdata = data.frame(fheight = m), interval = "confidence")
#prediction confidence interval
predict(fitC, newdata = data.frame(fheight = m), interval = "prediction")

#5
rm(list = ls())
data(mtcars)
y <- mtcars$mpg
x <- mtcars$hp
fit <- lm(mpg ~ hp, data = mtcars)
summary(fit)$coef
#p values are low for hp, so we reject the idea that hp has 0 influence on mpg

#6
sumCoef <- summary(fit)$coefficients
sum
predict(fit, newdata = data.frame(hp = mean(hp)), interval = "confidence")
sumCoef[1, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[1,2]
#[1] -294.4870 -224.7649

#now the slope
sumCoef[2, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[2,2]
#or
confint(fit)

#8 confint for the centered data
fitC <- lm(mpg ~ I(hp - mean(hp)), data = mtcars)
confint(fitC)

#9
m <- mean(mtcars$hp)
predict(fitC, newdata = data.frame(hp = m), interval = "confidence")

#10
predict(fitC, newdata = data.frame(hp = m), interval = "prediction")

#11 - plot the confidence interval
DF <- mtcars

pint <- predict(fit, newdata = data.frame(hp = DF$hp), interval = "prediction")
cint <- predict(fit, newdata = data.frame(hp = DF$hp), interval = "confidence")

DF <- data.frame(cbind(DF$mpg, DF$hp, pint, cint))
colnames(DF) <- c("mpg", "hp", "pint", "plwr", "pupr", "cint", "clwr", "cupr")

ggplot(DF, aes(x = hp, y = mpg)) + geom_point() + 
        geom_ribbon(ymax = DF$pupr, ymin = DF$plwr, alpha = 0.3, fill = "blue") +
        geom_ribbon(ymax = DF$cupr, ymin = DF$clwr, alpha = 0.3, fill = "red") +
        geom_smooth(method = "lm", se = FALSE, color = "black")
#from https://github.com/Jeffalltogether/RegressionModels/blob/master/Week2_01152016.R

mean(galton$child)
