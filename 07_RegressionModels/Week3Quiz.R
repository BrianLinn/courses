#1
rm(list = ls())
data(mtcars)
df <- as.data.frame(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = df)

summary(fit)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  33.990794  1.8877934 18.005569 6.257246e-17
# factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
# factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
# wt           -3.205613  0.7538957 -4.252065 2.130435e-04
#the change in estimate from 4 to 8 is -6.070860

#2
rm(list = ls())
data(mtcars)
df <- as.data.frame(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = df)
#now create model without confounder
fit2 <- update(fit, mpg ~ factor(cyl))
summary(fit)$coef
summary(fit2)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  33.990794  1.8877934 18.005569 6.257246e-17
# factor(cyl)6 -4.255582  1.3860728 -3.070244 4.717834e-03
# factor(cyl)8 -6.070860  1.6522878 -3.674214 9.991893e-04
# wt           -3.205613  0.7538957 -4.252065 2.130435e-04
# > summary(fit2)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)   26.663636  0.9718008 27.437347 2.688358e-22
# factor(cyl)6  -6.920779  1.5583482 -4.441099 1.194696e-04
# factor(cyl)8 -11.563636  1.2986235 -8.904534 8.568209e-10
#Holding weight constant, cylinder appears to have less of an impact on mpg
#than if weight is disregarded.


#3
rm(list = ls())
data(mtcars)
df <- as.data.frame(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = df)
#now model weight as an interaction term - multiply instead of add w
fit2 <- lm(mpg ~ factor(cyl) * wt, data = df)
anova(fit, fit2)
# Analysis of Variance Table
# 
# Model 1: mpg ~ factor(cyl) + wt
# Model 2: mpg ~ factor(cyl) * wt
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     28 183.06                           
# 2     26 155.89  2     27.17 2.2658 0.1239
#the p value of 0.1239 is above 5%, so we fail to reject Ho
#he P-value is larger than 0.05. So, according to our criterion, we would fail 
#to reject, which suggests that the interaction terms may not be necessary.

#4
rm(list = ls())
data(mtcars)
df <- as.data.frame(mtcars)
fit <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)  33.990794   1.887793 18.005569 6.257246e-17
# I(wt * 0.5)  -6.411227   1.507791 -4.252065 2.130435e-04
# factor(cyl)6 -4.255582   1.386073 -3.070244 4.717834e-03
# factor(cyl)8 -6.070860   1.652288 -3.674214 9.991893e-04
#Answer

#One unit of the weight variable equals 1000 lbs. Multiplying wt*0.5 doubles 
#the coeffiecient, which corresponds to the estimated expected change in MPG per
#one ton increase in weight for a specific number of cylinders (4, 6, 8).


#5
rm(list = ls())
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(hatvalues(fit))
#[1] 0.9945734


#6
rm(list = ls())
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit <- lm(y ~ x)
max(abs(dfbetas(fit)[,2]))
#max slope = 133.8226