#Binary GLMs exercises
rm(list = ls())
data("Seatbelts")
df <- as.data.frame(Seatbelts)
df <- mutate(df,
             pp = PetrolPrice - mean(PetrolPrice),
             mm = kms / 1000,
             mmc = mm - mean(mm),
             deathBin = ifelse(df$DriversKilled > 119, 1, 0)
)
fitBglm <- glm(deathBin ~ mmc + pp + law, data = df, family = binomial)
round(summary(fitBglm)$coef, 3)
# Call:
#         glm(formula = deathBin ~ mmc + pp + law, family = binomial, data = df)
# 
# Deviance Residuals: 
#         Min       1Q   Median       3Q      Max  
# -1.4893  -1.1234  -0.7461   1.0878   1.6708  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)  
# (Intercept)   0.024314   0.160775   0.151   0.8798  
# mmc          -0.002938   0.059848  -0.049   0.9608  
# pp          -34.199520  13.940264  -2.453   0.0142 *
#         law          -0.615522   0.577808  -1.065   0.2868  
# ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 266.09  on 191  degrees of freedom
# Residual deviance: 253.62  on 188  degrees of freedom
# AIC: 261.62
# 
# Number of Fisher Scoring iterations: 4

#logit scale odds of greater than 119 drivers killed is -.616 lower than before law
#mmc - -.003 means that we estikmate logit scale change to be -.003 lower for 1000 increase in kms
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)    0.024      0.161   0.151    0.880
# mmc           -0.003      0.060  -0.049    0.961
# pp           -34.200     13.940  -2.453    0.014
# law           -0.616      0.578  -1.065    0.287
table(df$deathBin)

#exp law coef
exp(-0.615522)
#0.54 - odds ratio of after the law to before the law - or .54 = 1-.54 = 46% decrease in odds of
#greater than 119 drivers being killed in a month - holding other vars constant

#exp mmc
1 - exp(0.002938)
#1] -0.00294232

rm(list = ls())
fitBglm <- glm(deathBin ~ mmc + pp + law, data = df, family = binomial)
fitB1 <- update(fitBglm, deathBin ~ law, data = df, family = binomial)
fitB2 <- update(fitBglm, deathBin ~ law + pp, data = df, family = binomial)
fitB3 <- update(fitBglm, deathBin ~ law + pp + mmc, data = df, family = binomial)
anova(fitB1, fitB2, fitB3)
# Model 1: deathBin ~ law
# Model 2: deathBin ~ law + pp
# Model 3: deathBin ~ law + pp + mmc
# Resid. Df Resid. Dev Df Deviance
# 1       190     260.40            
# 2       189     253.62  1   6.7760
# 3       188     253.62  1   0.0024

#compare to chi-aquared values - cut off around 4 for signifigance

summary(fitB1)$coef
summary(fitB2)$coef
summary(fitB3)$coef

# Estimate Std. Error    z value   Pr(>|z|)
# (Intercept)  0.08288766  0.1539783  0.5383074 0.59036483
# law         -1.12434153  0.4991985 -2.2522935 0.02430373
# > summary(fitB2)$coef
# Estimate Std. Error    z value  Pr(>|z|)
# (Intercept)   0.02561272  0.1585750  0.1615181 0.8716854
# law          -0.62602825  0.5367204 -1.1663956 0.2434546
# pp          -34.37372001 13.4887754 -2.5483203 0.0108243
# > summary(fitB3)$coef
# Estimate  Std. Error     z value   Pr(>|z|)
# (Intercept)   0.024313512  0.16077499  0.15122695 0.87979669
# law          -0.615522450  0.57780755 -1.06527242 0.28675267
# pp          -34.199520345 13.94026391 -2.45329074 0.01415559
# mmc          -0.002938343  0.05984816 -0.04909663 0.96084229

#-1.12434153 w/ one variable - decrease in odds
#-0.62602825 - w/ two vars - and pp is significant, and makes law insignificant
#-0.615522450 - little change to law - not significant with these two. 2nd model
#is best as it provides least chance for bias and includes correlated regressors


##Count data
##Poisson distributions
par(mfrow = c(1, 3))
plot(0 : 10, dpois(0 : 10, lambda = 2), type = "h", frame = FALSE)
plot(0 : 20, dpois(0 : 20, lambda = 10), type = "h", frame = FALSE)
plot(0 : 200, dpois(0 : 200, lambda = 100), type = "h", frame = FALSE)
##tends to approach normal with more samples

load("./data/gaData.rda")
gaData$julian = julian(gaData$date)
head(gaData)

plot(gaData$julian,
     gaData$visits,
     pch=19,
     col="darkgrey",
     xlab="Julian",
     ylab="Visits")

plot(gaData$julian,
        gaData$visits,
        pch=19,
        col="darkgrey",
        xlab="Julian",
        ylab="Visits")
lm1 = lm(gaData$visits ~ gaData$julian)
abline(lm1,col="red",lwd=3)


round(exp(coef(lm(I(log(gaData$visits + 1)) ~ gaData$julian))), 5)
# (Intercept) gaData$julian 
# 0.00000       1.00231 
#model expects a .02 percent increase in geometric mean daily web hits

par(mfrow = c(1,1))
plot(gaData$julian,
     gaData$visits,
     pch=19,
     col="darkgrey",
     xlab="Julian",
     ylab="Visits")
glm1 = glm(gaData$visits ~ gaData$julian,family="poisson")
abline(lm1,col="red",lwd=3)
lines(gaData$julian, glm1$fitted,col="blue",lwd=3)
#data with fitted poisson regression line

#mean-variance relationship illustrated
plot(glm1$fitted, glm1$residuals, pch = 19, col = "grey", ylab = "Residuals",
     xlab = "Fitted")
abline(lm1, col = "red", lwd = 3)

#if mean variance relatioship does not hold
glm2 <- glm(visits ~ julian, family = "quasipoisson", data = gaData)

#CI as a percentage
100 * (exp(confint(glm2)) - 1)[2, ]
#        2.5 %    97.5 % 
# julian 0.2072924 0.2520376 

#rates in poisson regression include temporal or sample size as ln offset in model
glm3 <- glm(simplystats ~ julian(gaData$date), offset = log(visits + 1), 
            family = "poisson", data = gaData)

plot(julian(gaData$date), glm3$fitted, col = "blue", pch = 19)
points(julian(gaData$date), glm1$fitted, col = "red", pch = 19)


##Count exercises
rm(list = ls())
data(Seatbelts)
seatbelts <- as.data.frame(Seatbelts)
library(dplyr)
seatbelts <- mutate(seatbelts, 
                    pp = (PetrolPrice - mean(PetrolPrice))/sd(PetrolPrice),
                    mm  = kms/1000,
                    mmc = mm - mean(mm))

#1
fit <- glm(DriversKilled ~ mmc + pp + law, family = "poisson", data = seatbelts)
round(summary(fit)$coef, 3)
# (Intercept)    4.820      0.007 676.243        0
# mmc           -0.010      0.003  -3.818        0
# pp            -0.055      0.007  -7.643        0
# law           -0.115      0.026  -4.495        0
1 - exp(-0.114877106) #the law coefficient unrounded
#[1] 0.1085243
#by exponentiating we get the % change in the drivers killed instead of 
#%change of the log of drives killed
#the model expects around 11% decrease in expected drivers killed after the law was enacted

1 - exp(-0.008189793) #mmc coef
#[1] 0.008156348
#the model expects less than 1% decrease in expected drivers killed per 1000 increase in kms

#the intercept is expected drivers killed on log scale
exp(4.819845137) #intercept
#[1] 123.9459
#the model expects ~124 drivers killed for average miles driven(scaled to 1000),
#and average petrol price, and at the time when the law was 0 - not enacted


#2
summary(fit)$coef
fit2 <- lm(log(DriversKilled) ~ mmc + pp + law, data = seatbelts)
summary(fit2)$coef
# Estimate  Std. Error    t value      Pr(>|t|)
# (Intercept)  4.805409776 0.014411832 333.435035 1.381656e-262
# mmc         -0.008189793 0.005325983  -1.537705  1.258020e-01
# pp          -0.053968063 0.014813218  -3.643237  3.481611e-04
# law         -0.131450856 0.048212881  -2.726468  7.007200e-0
1 - exp(-0.131450856) #law coef
#[1] 0.1231776
#the model expects a 12 percent decrease in geometric mean of 
#drivers killed after the law is enacted

1 - exp(summary(fit2)$coef[3, 1]) #the pp coef
#[1] 0.05253763
#the model expects a 5% decrease in geometric mean of 
#drivers killed per 1 sd increase in petrol price


1 - exp(summary(fit2)$coef[2, 1])
#[1] 0.008156348
#the model expects less than 1% decrease in geometric mean 
#expected drivers killed per 1000 increase in kms

#3
fit <- glm(DriversKilled ~ mmc + pp + law, family = "poisson", data = seatbelts)
fit3 <- glm(DriversKilled ~ mmc + pp + law, family = "poisson", data = seatbelts,
            offset = log(drivers))

summary(fit3)$coef
# Estimate  Std. Error     z value  Pr(>|z|)
# (Intercept) -2.612798146 0.007122545 -366.834931 0.0000000
# mmc          0.003377675 0.002630717    1.283937 0.1991640
# pp          -0.007255064 0.007199577   -1.007707 0.3135952
# law          0.028484328 0.025512651    1.116479 0.2642173

#the offset models log of expected drivers killed /  total drivers killed
##how did proportion of drivers killed 
#without the offset it models log of expectd value
##what happened to the total drivers killed 

#now the law is flipped
exp(summary(fit3)$coef[4, 1]) #law coef
#[1] 1.028894
#represents a 3% increase in  drivers killed among those injured or killed 
#after law was introduced
#in contrast to decrease in driverskilled when not compared to all those injured or killed
#but the total driver population

#4
fit3 <- glm(DriversKilled ~ law + mmc + pp, family = "poisson", data = seatbelts)
fit2 <- update(fit3, DriversKilled ~ law + mmc)
fit1 <- update(fit3, DriversKilled ~ law)
anova(fit1, fit2, fit3)
# Analysis of Deviance Table
# 
# Model 1: DriversKilled ~ law
# Model 2: DriversKilled ~ law + mmc
# Model 3: DriversKilled ~ law + mmc + pp
# Resid. Df Resid. Dev Df Deviance
# 1       190     870.06            
# 2       189     836.81  1   33.249
# 3       188     778.32  1   58.490


#33.249 and 58.490 indicate signifigance
summary(fit3)$coef
summary(fit2)$coef
summary(fit1)$coef
# Estimate  Std. Error    z value     Pr(>|z|)
# (Intercept)  4.819845137 0.007127388 676.242884 0.000000e+00
# law         -0.114877106 0.025557951  -4.494770 6.964526e-06
# mmc         -0.009980975 0.002614002  -3.818274 1.343887e-04
# pp          -0.055361338 0.007243262  -7.643150 2.119715e-14
# > summary(fit2)$coef
# Estimate  Std. Error    z value     Pr(>|z|)
# (Intercept)  4.82675836 0.007041169 685.505293 0.000000e+00
# law         -0.16239906 0.024695060  -6.576176 4.827016e-11
# mmc         -0.01459325 0.002530616  -5.766680 8.084830e-09
# > summary(fit1)$coef
# Estimate  Std. Error   z value     Pr(>|z|)
# (Intercept)  4.8352482 0.006856395 705.21727 0.000000e+00
# law         -0.2274727 0.021923993 -10.37552 3.204779e-25

#law from fit1 -0.2274727 - highly significant
#moves to -0.16239906 when mmc is added and signifigance decreses
