rm(list = ls())
library(swirl)
swirl()
Brian
6
3
7
6
B
dim(InsectSprays)
head(InsectSprays, 15)
sA
summary(InsectSprays[,2])
sapply(InsectSprays, class)
#6 variables so we need 6 dummy variables - Each will
# | indicate if a particular outcome (a count) is associated with a specific 
# factor or category (insect spray).

fit <- lm(count ~ spray, data = InsectSprays)
summary(fit)$coef
# Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)  14.5000000   1.132156 12.8074279 1.470512e-19
# sprayB        0.8333333   1.601110  0.5204724 6.044761e-01
# sprayC      -12.4166667   1.601110 -7.7550382 7.266893e-11
# sprayD       -9.5833333   1.601110 -5.9854322 9.816910e-08
# sprayE      -11.0000000   1.601110 -6.8702352 2.753922e-09
# sprayF        2.1666667   1.601110  1.3532281 1.805998e-01
est <- summary(fit)$coef[, 1]
mean(sA) #14.5
3
mean(sB) # 14.5 + 0.833 or 15.333

#now to omit the intercept from the model
nfit <- lm(count ~ spray - 1, data = InsectSprays)
summary(nfit)$coef
# Estimate Std. Error   t value     Pr(>|t|)
# sprayA 14.500000   1.132156 12.807428 1.470512e-19
# sprayB 15.333333   1.132156 13.543487 1.001994e-20
# sprayC  2.083333   1.132156  1.840148 7.024334e-02
# sprayD  4.916667   1.132156  4.342749 4.953047e-05
# sprayE  3.500000   1.132156  3.091448 2.916794e-03
# sprayF 16.666667   1.132156 14.721181 1.573471e-22

#now relevel to see what happens
spray2 <- relevel(InsectSprays$spray, "C")
fit2 <- lm(count ~ spray2, data = InsectSprays)
summary(fit2)$coef
# Estimate Std. Error  t value     Pr(>|t|)
# (Intercept)  2.083333   1.132156 1.840148 7.024334e-02
# spray2A     12.416667   1.601110 7.755038 7.266893e-11
# spray2B     13.250000   1.601110 8.275511 8.509776e-12
# spray2D      2.833333   1.601110 1.769606 8.141205e-02
# spray2E      1.416667   1.601110 0.884803 3.794750e-01
# spray2F     14.583333   1.601110 9.108266 2.794343e-13
#mean of sprayc is 2.08333
4
mean(sC)
#2.083333
4
#mean a is 12.42 + 2.08 = 14.5
#calculate the spray2B t value - spraybcoef - sprayccoef
(fit$coef[2] - fit$coef[3]) / 1.6011
2
brian_linn@calquake.com
oMANZAOegbQYCC31

#multivariate 3
swirl()
Brian
6
3
8
dim(hunger)
948
names(hunger)
#The Numeric column for a particular row tells us the percentage of children 
#under age 5 who were| underweight when that sample was taken.
fit <- lm(Numeric ~ Year, data = hunger)
summary(fit)$coef
1
3
2
lmF <- lm(Numeric[Sex == "Female"] ~ Year[Sex == "Female"], 
          data = hunger)
lmM <- lm(Numeric[Sex == "Male"] ~ Year[Sex == "Male"], 
          data = hunger)

lmBoth <- lm(Numeric ~ Year + Sex, data = hunger)
summary(lmBoth)
#Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 633.5283   120.8950   5.240 1.98e-07 ***
#Year         -0.3084     0.0604  -5.106 3.99e-07 ***
#SexMale       1.9027     0.8576   2.219   0.0267 *  

3
2
1
3
lmInter <- lm(Numeric ~ Year + Sex + (Sex*Year), data = hunger)
summary(lmInter)
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  603.50580  171.05519   3.528 0.000439 ***
#Year          -0.29340    0.08547  -3.433 0.000623 ***
#SexMale       61.94772  241.90858   0.256 0.797946    
# Year:SexMale  -0.03000    0.12087  -0.248 0.804022    

#percentag of hungry females at year 0 - 603.50580
#the annual change in percentage of hungry females? - -0.29340
#the annual change in percentage of hungry males? -0.29 + -0.03 = -0.32340
1
2
2
3

rm(list = ls())
library(swirl)
swirl()
Brian
6
3
9
fit <- lm(y ~ x, data = out2)
plot(fit, which = 1)
2 #outlier is only cause of non-linearity
3
#exclude the outlier by removing the first row of data
fitno <- lm(y ~ x, data = out2[-1, ])
plot(fitno, which = 1)
coef(fit) - coef(fitno)
head(dfbeta(fit))

#calculate residuals of second model - which excludes the outlier
resno <- out2[1, "y"] - predict(fitno, out2[1, ])
1 - resid(fit)[1] / resno
#this is the manual way to calculate hatvalues - the first value at least
head(hatvalues(fit))
#standardized residuals
sigma = sqrt(deviance(fit) / df.residual(fit))

#Compute fit's standardized residual
rstd <- resid(fit) / (sigma*sqrt(1-hatvalues(fit)))
#rstandard can do all of the above in one step
head(cbind(rstd, rstandard(fit)))

#A Scale-Location plot shows the square root of standardized residuals against fitted values. 
plot(fit, which = 3)

##qq plot of residuals v fit
plot(fit, which = 2)
2
2
#standardized residual for lm without outlier
sigma1 <- sqrt(deviance(fitno) / df.residual(fitno))
#studentized residual without outlier
resid(fit)[1]/(sigma1*sqrt(1-hatvalues(fit)[1]))
head(rstudent(fit))
#again the rstudent does in one step what the math did in the preceding steps

#calculate the outlier's Cook's distance
dy <- predict(fitno, out2) - predict(fit, out2)
sum(dy^2) / (2 * sigma^2)
cooks.distance(fit)[1]
plot(fit, which = 5)
1
brian_linn@calquake.com
szSbwM1Uz9HptKDk
