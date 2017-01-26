#1
#Consider the space shuttle data ?shuttle in the MASS library. Consider modeling the use of the autolander as the outcome (variable name use). Fit a logistic regression model with autolander (variable auto) use (labeled as "auto" 1) versus not (0) as predicted by wind sign (variable wind). Give the estimated odds ratio for autolander use comparing head winds, labeled as "head" in the variable headwind (numerator) to tail winds (denominator).
rm(list = ls())
library(MASS)
?shuttle
#use as outcome
head(shuttle)
shut <- as.data.frame(shuttle)
shut$useBin <- ifelse(shut$use == "auto", 1, 0)
fit <- glm(useBin ~ factor(wind) - 1, family = "binomial", data = shut) 

round(summary(fit)$coef, 3)
# Estimate Std. Error z value Pr(>|z|)
# factor(wind)head    0.251      0.178   1.410    0.158
# factor(wind)tail    0.283      0.179   1.586    0.113

exp(summary(fit)$coef)
# Estimate Std. Error  z value Pr(>|z|)
# factor(wind)head 1.285714   1.195033 4.097999 1.171626
# factor(wind)tail 1.327273   1.195484 4.882653 1.119419

round((exp(summary(fit)$coef[1, 1])) / (exp(summary(fit)$coef[2, 1])), 3)
#[1] 0.969


#2
#Consider the previous problem. Give the estimated odds ratio for autolander use comparing head winds (numerator) to tail winds (denominator) adjusting for wind strength from the variable magn.
fit2 <- glm(useBin ~ factor(wind) + factor(magn) - 1, family = "binomial", data = shut) 
exp(summary(fit2)$coef)
(exp(summary(fit2)$coef[1, 1])) / (exp(summary(fit2)$coef[2, 1]))
#[1] 0.9684981


#3
#If you fit a logistic regression model to a binary variable, for example use of the autolander, then fit a logistic regression model for one minus the outcome (not using the autolander) what happens to the coefficients?
fit3 <- glm(1 - useBin ~ factor(wind) - 1, family = "binomial", data = shut) 
round(summary(fit3)$coef, 3)
# Estimate Std. Error z value Pr(>|z|)
# factor(wind)head   -0.251      0.178  -1.410    0.158
# factor(wind)tail   -0.283      0.179  -1.586    0.113

#the signs are reversed

#4
#Consider the insect spray data InsectSprays. Fit a Poisson model using spray as a factor level. Report the estimated relative rate comapring spray A (numerator) to spray B (denominator).
rm(list = ls())
data("InsectSprays")
spray <- as.data.frame(InsectSprays)
?InsectSprays
fit <- glm(count ~ factor(spray) - 1, family = poisson, data = spray)
summary(fit)$coef
# Estimate Std. Error   z value      Pr(>|z|)
# factor(spray)A 2.6741486 0.07580980 35.274443 1.448048e-272
# factor(spray)B 2.7300291 0.07372098 37.031917 3.510670e-300
# factor(spray)C 0.7339692 0.19999987  3.669848  2.426946e-04
# factor(spray)D 1.5926308 0.13018891 12.233229  2.065604e-34
# factor(spray)E 1.2527630 0.15430335  8.118832  4.706917e-16
# factor(spray)F 2.8134107 0.07071068 39.787636  0.000000e+00

exp(summary(fit)$coef[1, 1]) / exp(summary(fit)$coef[2, 1])
#[1] 0.9456522


#5
rm(list = ls())
data("InsectSprays")
spray <- as.data.frame(InsectSprays)
?InsectSprays
fit <- glm(count ~ factor(spray) - 1, family = poisson, data = spray, offset = log(count + 1))
fit2 <- glm(count ~ factor(spray) - 1, family = poisson, data = spray, offset = log(10) + log(count + 1))
summary(fit)$coef
summary(fit2)$coef
# Estimate Std. Error    z value   Pr(>|z|)
# factor(spray)A -0.06669137 0.07580980 -0.8797196 0.37901120
# factor(spray)B -0.06317890 0.07372098 -0.8570003 0.39144471
# factor(spray)C -0.39204209 0.19999994 -1.9602110 0.04997113
# factor(spray)D -0.18514243 0.13018891 -1.4221060 0.15499548
# factor(spray)E -0.25131443 0.15430335 -1.6287036 0.10337577
# factor(spray)F -0.05826891 0.07071068 -0.8240468 0.40991297
# > summary(fit2)$coef
# Estimate Std. Error   z value      Pr(>|z|)
# factor(spray)A -2.369276 0.07580980 -31.25290 2.038695e-214
# factor(spray)B -2.365764 0.07372098 -32.09078 5.928009e-226
# factor(spray)C -2.694627 0.19999994 -13.47314  2.250863e-41
# factor(spray)D -2.487728 0.13018891 -19.10860  2.141463e-81
# factor(spray)E -2.553900 0.15430335 -16.55116  1.570458e-61
# factor(spray)F -2.360854 0.07071068 -33.38752 2.080759e-244

# coefficient estimate remains unchanged 0.075

#6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knots<-c(0)
splineTerms<-sapply(knots,function(knot) (x>knot)*(x-knot))
xmat<-cbind(1,x,splineTerms)
fit<-lm(y~xmat-1)
yhat<-predict(fit)
(yhat[10]-yhat[6])/4
summary(fit)$coef
plot(x,y)
lines(x,yhat,col="red")


(yhat[11]-yhat[7])/4
#1.013067