rm(list = ls())
#1
#Give a P-value for the two sided hypothesis test of whether β1 from a linear
#regression model is 0 or not.
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y ~ x)
summary(fit)$coefficients
#Estimate Std. Error   t value   Pr(>|t|)
#(Intercept) 0.1884572  0.2061290 0.9142681 0.39098029
#x           0.7224211  0.3106531 2.3254912 0.05296439
#0.05296439


#2 using above data - give the estimate of the residual standard deviation.
summary(fit)$sigma
#[1] 0.2229981

#3
data(mtcars)
fit <- lm(mpg ~ wt, data = mtcars)
m <- mean(mtcars$wt)
predict(fit, newdata = data.frame(wt = m), interval = "confidence")


#4
summary(fit)
?mtcars

#5 wt = 3ooo lbs /1000 lbs = 3
confint(fit)
predict(fit, newdata = data.frame(wt = 3), interval = "prediction")

#6
summary(fit)
fit <- lm(mpg ~ I(wt/2), data = mtcars)
confint(fit)

#7
#effect of converting the units from cm to meters
fit <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ I(wt/100), data = mtcars)
summary(fit1)
summary(fit2)
#slope coefficient multiplied by 100


#8
#effect of adding constant to the predictor
c <- 3
fit <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ I(wt + c), data = mtcars)
fitCoef <- summary(fit)$coefficients
fit2Coef <- summary(fit2)$coefficients

beta01 <- fitCoef[1, 1]
beta02 <- fit2Coef[1, 1]

beta11 <- fitCoef[2, 1]
beta12 <- fit2Coef[2, 1]

c * beta11
c * beta12

#9
y <- mtcars$mpg
x <- mtcars$wt
fit1 <- lm(mpg ~ wt, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
1 - summary(fit1)$r.squared
#or
sse1 <- sum((predict(fit1) - mtcars$mpg)^2)
sse2 <- sum((predict(fit2) - mtcars$mpg)^2)
sse1/sse2
#or
sum(resid(fit)^2) / sum((y - mean(y))^2)
