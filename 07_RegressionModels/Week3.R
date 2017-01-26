#linear model with three variables
rm(list = ls())
n <- 100
x <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
##generating the data
y <- 1 + x + x2 + x3 + rnorm(n, sd = 0.1)
##getting the residuals - removing x2 and x3 from x1 and y
ey <- resid(lm(y ~ x2 + x3))
ex <- resid(lm(x ~ x2 + x3))

##fit the regression through the origin with the residuals
sum(ey * ex) / sum(ex ^ 2)
#[1] 1.008087
#verify this with lm
coef(lm(ey ~ ex - 1))
#1.008087 

#fitting the full model to show it agrees
coef(lm(y ~ x + x2 + x3))
#(Intercept)           x          x2          x3 
#1.0065368   ****1.0080874   0.9924284   1.0001105 

plot(y)

#1
data(Seatbelts)
head(Seatbelts)
names(Seatbelts)

df <- as.data.frame(Seatbelts)

fit <- lm(DriversKilled ~ kms + PetrolPrice, data = df)
round(summary(fit)$coef, 4)

#add centered data to the dataframe and then convert the kms to meters and center
library(dplyr)
df <- mutate(df, pp = (PetrolPrice - mean(PetrolPrice)) / sd(PetrolPrice),
             mm = kms/1000,
             mmc = mm - mean(mm))

head(df)
#shows drivers killed at average petrol price and average kms driven
fit2 <- lm(DriversKilled ~ pp + mmc, data = df)
round(summary(fit2)$coef, 4)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 122.8021     1.6629 73.8503   0.0000
#pp           -7.8387     1.8055 -4.3414   0.0000
#mmc          -1.7495     0.6145 -2.8469   0.0049
#The -7 for petrol price meanst hat we expect 7 fewer deaths / 1 standerd deviation
#change in petrol price
#-2 fewer deaths for every 1000 kms driven

#2
mpp <- mean(df$PetrolPrice)
mkms <- mean(df$kms)

predict(fit, newdata = data.frame(PetrolPrice = mpp, kms = mkms))
#122.8021 driver deaths at mean kms and petrol price 

#3
y <- df$DriversKilled
x <- df$PetrolPrice
z <- df$kms

fitFull <- lm(y ~ x + z)

ey <- resid(lm(y ~ z))
ex <- resid(lm(x ~ z))
##fit the regression through the origin with the residuals
sum(ey * ex) / sum(ex ^ 2)
#[1] -643.7895 which is the same as 
summary(lm(ey ~ ex -1))$coef
# Estimate Std. Error   t value     Pr(>|t|)
#ex -643.7895   147.5111 -4.364345 2.085664e-05
#now compare to the full model
round(summary(fitFull)$coef, 4)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept)  215.7461    14.6656 14.7110   0.0000
####x           -643.7895   148.2896 -4.3414   0.0000
#z             -0.0017     0.0006 -2.8469   0.0049
#verify this with lm
coef(lm(ey ~ ex - 1))
#ex 
#-643.7895


#4
dk <- df$DriversKilled
kms <- df$DriversKilled
pp <- df$PetrolPrice
#create the full model with pp and kms regressed over dk
fitFull <- lm(dk ~ pp + kms)

#now regress out pp for dk with an intercept
edk <- resid(lm(dk ~ pp))
ekms <- resid(lm(kms ~ pp))

# now fit a regression through the origin for both residuals
round(summary(lm(edk ~ ekms - 1))$coef, 4)
round(summary(fitFull)$coef, 4)

#multivariable regression examples
rm(list = ls())
require(datasets)
require(GGally)
require(ggplot2)
data(swiss)
?swiss

#basic scatterplot - of course he uses a function that is deprecated so the code below
#fails outright and it was an attemp to modify the even more inaccurate code from the text
#and the lecture
g <- ggpairs(swiss, lower = list(continuous = "smooth"), wrapp(funcVal = ggpairs, params = c(method = "loess")))
?ggpairs
?ggally_NAME

#illustrating how adjustment can reverse the sign of an effect
n <- 100
x2 <- 1 : n
x1 <- .01 * x2 + runif(n, -0.1, 0.1)
y <- -x1 + x2 + rnorm(n, sd = 0.01)
summary(lm(y ~ x1))$coef #x1 95.466777
summary(lm(y ~ x1 + x2))$coef #x1 -0.971181892 

#now plot the regressions
#first create the data frame to use in ggplot
dat <- data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g <- ggplot(dat, aes(y = y, x = x1, colour = x2))
g <- g +
        geom_point(colour = "grey50", size = 5) + 
        geom_smooth(method = lm, se = FALSE, colour = "black") + 
        geom_point(size = 4)
g
#as y goes up so does x2, and as x1 goes up so does x2 - this can lead to confounding
#now plot the residuals
g <- ggplot(dat, aes(y = ey, x = ex1, colour = x2))
g <- g +
        geom_point(colour = "grey50", size = 5) + 
        geom_smooth(method = lm, se = FALSE, colour = "black") + 
        geom_point(size = 4)
g
#this shows the now negative relationship between y and x1
#x2 is clearly shown to be unrelated to x1 as the gradient follows no pattern

#wha thappens if an unneccessary variable is regressed
z <- swiss$Agriculture + swiss$Education
lm(Fertility ~ . + z, data = swiss)
#Intercept)       Agriculture       Examination         Education          Catholic  
#66.9152           -0.1721           -0.2580           -0.8709            0.1041  
#Infant.Mortality                 z  
#1.0770                         NA
#the unnecessary variable does not affect the agriculture coeffiecient and r 
#returns an NA for the variable as it is an axact linear combination of other variables


#Dummy variables
rm(list = ls())
require(datasets)
require(stats)
require(ggplot2)
data(InsectSprays)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g
#now set Spray as the reference level - R chooses first in alpha sort unless specified in relevel
summary(lm(count ~ spray, data = InsectSprays))$coef
# Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)  14.5000000   1.132156 12.8074279 1.470512e-19
# sprayB        0.8333333   1.601110  0.5204724 6.044761e-01
# sprayC      -12.4166667   1.601110 -7.7550382 7.266893e-11
# sprayD       -9.5833333   1.601110 -5.9854322 9.816910e-08
# sprayE      -11.0000000   1.601110 -6.8702352 2.753922e-09
# sprayF        2.1666667   1.601110  1.3532281 1.805998e-01
#Therefore, 0.8333 is the estimated mean comparing Spray B to Spray A (as B - A),
#-12.4167 compares Spray C to Spray A (as C - A) and so on. The inferencial 
#statistics: standard errors, t value and P-value all correspond to those 
#comparisons. The intercept, 14.5, is the mean for Spray A. So, its inferential
#statistics are testing whether or not the mean for Spray A is zero

#if we drop the intercept, the a reference is no longer redundant
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
# Estimate Std. Error   t value     Pr(>|t|)
# sprayA 14.500000   1.132156 12.807428 1.470512e-19
# sprayB 15.333333   1.132156 13.543487 1.001994e-20
# sprayC  2.083333   1.132156  1.840148 7.024334e-02
# sprayD  4.916667   1.132156  4.342749 4.953047e-05
# sprayE  3.500000   1.132156  3.091448 2.916794e-03
# sprayF 16.666667   1.132156 14.721181 1.573471e-22
#now the means for each level are displayed
#compare to the emprical means
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))
# A tibble: 6 × 2
# spray        mn
# <fctr>     <dbl>
# 1      A 14.500000
# 2      B 15.333333
# 3      C  2.083333
# 4      D  4.916667
# 5      E  3.500000
# 6      F 16.666667

#now relevel with c as the reference 
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, InsectSprays))$coef
# Estimate Std. Error  t value     Pr(>|t|)
# (Intercept)  2.083333   1.132156 1.840148 7.024334e-02
# spray2A     12.416667   1.601110 7.755038 7.266893e-11
# spray2B     13.250000   1.601110 8.275511 8.509776e-12
# spray2D      2.833333   1.601110 1.769606 8.141205e-02
# spray2E      1.416667   1.601110 0.884803 3.794750e-01
# spray2F     14.583333   1.601110 9.108266 2.794343e-13
#now the intercept is the mean for spray c and all of the coeffiecients are 
#interpreted as related to C - 12.147 is A to C


#now to show how dummy variables work
library(dplyr)
swiss <- mutate(swiss, CatholicBin = 1 * (Catholic > 50))
g <- ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g <- g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g <- g + xlab("% in Agriculture") + ylab("Fertility")
g
summary(lm(Fertility ~ Agriculture, data = swiss))$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 60.3043752 4.25125562 14.185074 3.216304e-18
# Agriculture  0.1942017 0.07671176  2.531577 1.491720e-02

summary(lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss))$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)          60.8322366  4.1058630 14.815944 1.032493e-18
# Agriculture           0.1241776  0.0810977  1.531210 1.328763e-01
# factor(CatholicBin)1  7.8843292  3.7483622  2.103406 4.118221e-02
#Thus, 7.8843 is the estimated change in the intercept in the expected relationship between Agriculture
#and Fertility going from a non-Catholic majority province to a Catholic majority.
#Thus, 7.8843 is the estimated change in the intercept in the expected relationship between Agriculture
# #and Fertility going from a non-Catholic majority province to a Catholic majority.
# The interaction term 0.9891 is the estimate change in the slope. The estimated intercept in non-
#Catholic provinces is 62.04993 while the estimated intercept in Catholic provinces is 62.04993 +
#2.85770. The estimated slope in non-Catholic majority provinces is 0.09612 while it is 0.09612 +
#0.08914 for Catholic majority provinces

##parallell lines
fit <- summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))
fit
g1 <- g
g1
#this plots the line for reference - majority proetstant
g1 <- g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2,
                       color = 2)
g1
#to plot for catholic provinces we add refernce slope and intercept to regressor
#slope and intercept
g1 <- g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3],
                       slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1



#exercises multivariable axamples and tricks
#1
rm(list = ls())
data(Seatbelts)

df <- as.data.frame(Seatbelts)

fit <- lm(DriversKilled ~ kms + PetrolPrice, data = df)
round(summary(fit)$coef, 4)
#this information relates to 0 kms or petrol, so it is of little interest
#to make it more interesting
#add centered data to the dataframe and then convert the kms to meters and center
library(dplyr)
df <- mutate(df, pp = (PetrolPrice - mean(PetrolPrice)) / sd(PetrolPrice),
             mm = kms/1000,
             mmc = mm - mean(mm))
##petrol - mean petrol to center; divide by sd scales the data
## kms divide by 1000 scales the meters to megameters haha - so now model will
#show change in 1000 meters driven instead of 10,000
##mm - mean mm centers the mm(kms) data
head(df)
#shows drivers killed at average petrol price and average kms driven
fit2 <- lm(DriversKilled ~ pp + mmc, data = df)
round(summary(fit2)$coef, 4)
#Estimate Std. Error t value Pr(>|t|)
#(Intercept) 122.8021     1.6629 73.8503   0.0000
#pp           -7.8387     1.8055 -4.3414   0.0000
#mmc          -1.7495     0.6145 -2.8469   0.0049
#The -7 for petrol price meanst hat we expect 7 fewer deaths / 1 standerd deviation
#change in petrol price
#-2 fewer deaths for every 1000 kms driven

#2
#same as above using the log deaths for outcome
fit2L <- lm(I(log(DriversKilled)) ~ pp + mmc, data = df)
summary(fit2L)$coef
# Estimate  Std. Error    t value      Pr(>|t|)
# (Intercept)  4.78966306 0.013426810 356.723817 2.737888e-269
# pp          -0.06412578 0.014579039  -4.398492  1.818005e-05
# mmc         -0.01400794 0.004962149  -2.822959  5.267843e-03
#all estimates on log scale
#
1 - exp(-0.06412578)
#normalized petrol price interpreted as a 6% decrese in geometric mean of drivers killed
#for evrey one standard deviation of increase in petrol price - holding kms constant

1 - exp(-0.01400794)
#for every additional 1000 kms, model estimates 1% decrease in geometric mean of drivers killed
#holding petrol price constant

#3
fit2dummy <- lm(DriversKilled ~ pp + mmc + law, data = df)
?Seatbelts
# summary(fit2dummy)$coef
# Estimate Std. Error   t value      Pr(>|t|)
# (Intercept) 124.226311  1.8012324 68.967399 1.976641e-135
# pp           -6.919949  1.8513987 -3.737687  2.463128e-04
# mmc          -1.223318  0.6656567 -1.837761  6.767594e-02
# law         -11.889202  6.0257850 -1.973055  4.995497e-02
#the law is 0/1 - so intercept is expected number of drivers killed for average petrol price 
#average kms driven and before the law was in effect (0)
# law interprets as 12 fewer deaths per month associated withthe law holding petrol
#and kms constant
#other variables interpreted the same, but now we hold the other variables constant

fit2dummyFac <- lm(DriversKilled ~ pp + mmc + factor(law), data = df)
summary(fit2dummyFac)$coef
#results are identical - but the law value is now a factor

#4
df <- as.data.frame(Seatbelts)
df <- mutate(df, pp = (PetrolPrice - mean(PetrolPrice)) / sd(PetrolPrice),
             ppf = cut(df$PetrolPrice, breaks = 4),
             mm = kms/1000,
             mmc = mm - mean(mm))

fit <- lm(DriversKilled ~ ppf + mmc + law, data = df)
summary(fit)$coef
#  Estimate Std. Error   t value      Pr(>|t|)
# (Intercept)       136.631166  3.1201457 43.789996 6.550961e-100
# ppf(0.0941,0.107] -16.984492  4.5877019 -3.702179  2.815438e-04
# ppf(0.107,0.12]   -17.128461  4.4612511 -3.839385  1.689720e-04
# ppf(0.12,0.133]   -17.664649  8.5725686 -2.060602  4.073297e-02
# mmc                -1.655182  0.7277007 -2.274537  2.407517e-02
# law               -12.792231  5.9429925 -2.152490  3.264717e-02
table(df$PetrolPrice)

#5
##parallell lines
fit <- summary(lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss))
fit
g1 <- g
g1
#this plots the line for reference - majority proetstant
g1 <- g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2,
                       color = 2)
g1
#to plot for catholic provinces we add refernce slope and intercept to regressor
#slope and intercept
g1 <- g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3],
                       slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1


#Adjustment - Exercises
#1
rm(list = ls())
data("Seatbelts")
df <- as.data.frame(Seatbelts)
library(dplyr)
df <- mutate(df, pp = (PetrolPrice - mean(PetrolPrice)) / sd(PetrolPrice),
             mm = kms/1000,
             mmc = mm - mean(mm))
fit <- lm(DriversKilled ~ mmc + pp, data = df)
round(summary(fit)$coef,4)
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept) 122.8021     1.6629 73.8503   0.0000
# mmc          -1.7495     0.6145 -2.8469   0.0049
# pp           -7.8387     1.8055 -4.3414   0.0000
#the intercept of 122 - is the number of drivers killed at avg kms and avg petrolprice
#The kms coefficient is the slope of the regression - -1.7 deaths per 1000 kms driven
#the petrol price coefficient is -7.83 deaths per standard deviation increase in normalized pp

#2
fitNopp <- lm(DriversKilled ~ mmc, data = df)
round(summary(fitNopp)$coef, 4)
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 122.802083  1.7391997 70.60839 2.665611e-138
# mmc          -2.773787  0.5935049 -4.67357  5.596266e-06
#the kms coefficient with pp was -1.75
#but without the pp it is -2.774 - this is higher than with the inclusion of pp
#pp has confounding effect on kms and driver deaths
#signifigance from -6 to -3, but still significant

#3
fitNokms <- lm(DriversKilled ~ pp, data = df)
summary(fitNokms)$coef
summary(fit)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept) 122.802083   1.693656 72.507096 2.061333e-140
# pp           -9.812019   1.698084 -5.778288  3.044208e-08
#with kms the pp coef was -7.84, but without it is -9.81 - so kms has impact on pp
round(colMeans(df), 4)


#Adjustment
#putting regressors into the model to investigate the role of a third variable 
#on the relationship between the other two

#Experiment 1
#line: Yi = B0 + B1X + tT + e1
#now simulate the line
#define n as number of sims
n <- 1000
#define the t as 500 0s and 500 1s
t <- rep(c(0,1), c(n/2, n/2))
#define x as  1000 random uniform variables
set.seed(1234)
x <- c(runif(n/2), runif(n/2))
#define the intercept
beta0 <- 0
#define the slope 
beta1 <- 2
#define the test stat tau
tau <- 1
#define the sd
sigma <- .2
#code in the line's equation as y
y <- beta0 + beta1 * x + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
#plot the linear model with x predicting outcome y
abline(lm(y ~ x), lwd = 2)
#add the line of the first 500 y values and the second
abline(h = mean(y[1: n/2]), lwd = 3, col = "lightblue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "salmon")
#store the model in a variable
fit <- lm(y ~ x + t)
#add the fit lines for the coefficients
abline(coef(fit)[1], coef(fit)[2], lwd = 3, col = "lightblue")
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3, col = "salmon")
#now add the points
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue",
       cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", 
       cex = 2)
# The X variable is unrelated to group status
# * The X variable is related to Y, but the intercept depends
# on group status.
# * The group variable is related to Y.
# * The relationship between group status and Y is constant depending on X.
# * The relationship between group and Y disregarding X is about the same as holding X constant

instal
##RESIDUALS
rm(list = ls())
data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss)
plot(fit)
plot(resid(fit))

#plot grouped data with strongly visually separated outliers
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE, xlab = "X", ylab = "Y")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

## Upper left hand point has low leverage, low influence, outlies in a way not 
# conforming to the regression relationship.
## Lower left hand point has low leverage, low influence and is not to be an 
# outlier in any sense.
## Upper right hand point has high leverage, but chooses not to extert it and 
# thus would have low actual influence by conforming to the regresison relationship 
# of the other points.
## Lower right hand point has high leverage and would exert it if it were included in the fit.

##Influence
?influence.measures

#Case 1
set.seed(1234)
rm(list = ls())
n <- 100
set.seed(1234)
x <- c(10, rnorm(n))
set.seed(12345)
y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))
fit <- lm(y ~ x)
#look at the betas - checks for individual coefficient influences
round(dfbetas(fit)[1 : 10, 2], 3)
# 1      2      3      4      5      6      7      8      9     10 
# 5.485 -0.056  0.004 -0.069 -0.058  0.000 -0.079 -0.017  0.010  0.010
#first value has very large influence versus the other - the 10 is much larger 
#than the cloud of data
#the cooks distance and dffits show the same large influence for the 10
round(cooks.distance(fit)[1 : 10], 3)
# 1      2      3      4      5      6      7      8      9     10 
# 11.918  0.004  0.000  0.006  0.002  0.000  0.022  0.001  0.000  0.000 
round(dffits(fit)[1 : 10], 3)
# 1      2      3      4      5      6      7      8      9     10 
# 5.539  0.089  0.015 -0.109  0.068  0.000 -0.214  0.048 -0.030 -0.030 

#the leverage
round(hatvalues(fit)[1 : 10], 3)
# 1     2     3     4     5     6     7     8     9    10 
# 0.510 0.016 0.010 0.016 0.036 0.011 0.011 0.011 0.011 0.011 
#high leverage with high potential for influencing the fit

##Case 2
x <- rnorm(n); y <- x + rnorm(n, sd = .3)
x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
fit2 <- lm(y ~ x)
abline(fit2) 
#now the outlier is on the fit line, so how does this affect influence and leverage
round(dfbetas(fit2)[1 : 10, 2], 3)
# 1      2      3      4      5      6      7      8      9     10 
# 0.294 -0.017  0.086  0.010 -0.137  0.004 -0.006  0.069  0.119 -0.005 
round(hatvalues(fit2)[1 : 10], 3)
# 1     2     3     4     5     6     7     8     9    10 
# 0.202 0.010 0.022 0.011 0.026 0.010 0.013 0.011 0.027 0.012 
#the first value in the betas is well within the norm
#but the leverage is much larger than the others - high leverage - low influence

#Stefanski example
dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Ima\
ges/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)
summary(lm(V1 ~ . , data = dat))$coef
#very noisy plot with all vars as highly significant
#no clear pattern to discern, but lets how a residual plot can unearth a pattern
fit <- lm(V1 ~ . - 1, data = dat)
plot(predict(fit), resid(fit), pch = '.')
#shows a fun picture of a bird with the caption 'O RLY?' lol
#residuals can uncover patterns in the data

#Exercises
#1

rm(list = ls())
data("Seatbelts")
df <- as.data.frame(Seatbelts)
fit <- lm(DriversKilled ~ kms + PetrolPrice, data = df)
#now we need to center and scale the data for better measures
library(dplyr)
n <- nrow(df2)
p <- 4
#number of parameters, DK, mmc, pp, and law
df2 <- mutate(df, 
              pp = (PetrolPrice - mean(PetrolPrice)/ sd(PetrolPrice)),
              mm = kms / 1000,
              mmc = mm - mean(mm))
fit2 <- lm(DriversKilled ~ mmc + pp + law, data = df2)

#2
#directly calculate the residual variance
sum(resid(fit2)^2 / (n - p))
#let r do it for you
summary(fit2)$sigma^2
#both result in 522.8903

#3
round(dffits(fit2)[1: 10], 3)
round(dfbetas(fit2)[1 : 10,], 3)
round(hatvalues(fit2)[1 : 10], 3)
plot(dffits(fit2))
plot(dfbetas(fit2)[, 2]) #plots the second column - kms only
plot(cooks.distance(fit2))
#        1      2      3      4      5      6      7      8      9     10 
#dffits        -0.199 -0.361 -0.200 -0.248 -0.050 -0.099 -0.059 -0.067 -0.097  0.027
#dfbetas        -0.051 -0.086 -0.043 -0.035 -0.006 -0.006 -0.014 -0.015 -0.027  0.006
#hatvalues        0.031 0.045 0.023 0.016 0.012 0.009 0.007 0.006 0.011 0.014 
plot(fit2)


#Multiple Variables and Model Selection
#Exercises 
#first create the model with 3 variables as regressors
rm(list = ls())
data("Seatbelts")
df <- as.data.frame(Seatbelts)
library(dplyr)
#center and scale the data to make the inferences more interesting and useful
df <- mutate(df,
             pp = (PetrolPrice - mean(PetrolPrice))/ sd(PetrolPrice),
             mm = kms / 1000,
             mmc = mm - mean(mm))
head(df)
#determine if law is important
fit0 <- lm(DriversKilled ~ law, data = df)
fit1 <- update(fit0, DriversKilled ~ law + mmc)
fit2 <- update(fit0, DriversKilled ~ law + pp)
fit3 <- update(fit0, DriversKilled ~ law + mmc + pp)
anova(fit0, fit1, fit2, fit3)
rbind(summary(fit0)$coef[2, ],
      summary(fit1)$coef[2, ],
      summary(fit2)$coef[2, ],
      summary(fit3)$coef[2, ])
# Estimate Std. Error   t value     Pr(>|t|)
# [1,] -25.60895   5.341655 -4.794198 3.288375e-06
# [2,] -17.55372   6.028888 -2.911602 4.028394e-03
# [3,] -16.32618   5.555579 -2.938700 3.706585e-03
# [4,] -11.88920   6.025785 -1.973055 4.995497e-02
#all of the variables have p that are very significant, so law should be included

#model selection - swiss data
data(swiss);
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
a <- summary(fit1)$cov.unscaled[2,2]
fit2 <- update(fit1, Fertility ~ Agriculture + Examination)
fit3 <- update(fit1, Fertility ~ Agriculture + Examination + Education)
c(summary(fit2)$cov.unscaled[2,2],
    summary(fit3)$cov.unscaled[2,2]) / a
#[1] 1.891576 2.089159
#inclusion of examination increases variance of agriculture effect by 89.2% while
#adding education and examination added 108.9% increase
anova


install.packages("car")
library(car)
fit <- lm(Fertility ~ ., data = swiss)
#variance inflation 
vif(fit)
# Agriculture      Examination        Education         Catholic Infant.Mortality 
# 2.284129         3.675420         2.774943         1.937160         1.107542
#standard error of agriculture is 2 - so double what it would be if it were
#orthogonal to all the other regressors
#infant mortality not related to other variables, so it has low vif

#now to see the sd inflation
sqrt(vif(fit))
# Agriculture      Examination        Education         Catholic Infant.Mortality 
# 1.511334         1.917138         1.665816         1.391819         1.052398 
