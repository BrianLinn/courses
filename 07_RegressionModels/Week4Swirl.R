remove(list = ls())
library(swirl)
swirl()
Brian
6
3
10
makelms <- function(x1, x2, x3){
        # Simulate a dependent variable, y, as x1
        # plus a normally distributed error of mean 0 and 
        # standard deviation .3.
        y <- x1 + rnorm(length(x1), sd = .3)
        # Find the coefficient of x1 in 3 nested linear
        # models, the first including only the predictor x1,
        # the second x1 and x2, the third x1, x2, and x3.
        c(coef(lm(y ~ x1))[2], 
          coef(lm(y ~ x1 + x2))[2], 
          coef(lm(y ~ x1 + x2 + x3))[2])
}

# Regressor generation process 1.
rgp1 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducibility
        set.seed(4321)
        # Point A
        x1 <- rnorm(n)
        x2 <- rnorm(n)
        x3 <- rnorm(n)
        # Point B
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
        round(apply(betas, 1, var), 5)
}
#the results are the variance of x1s coeffiecient- for the uncorrelated rgp1
#note that the vars are all the same and low
# x1      x1      x1 
#0.00110 0.00111 0.00112 
# Regressor generation process 2.
rgp2 <- function(){
        print("Processing. Please wait.")
        # number of samples per simulation
        n <- 100
        # number of simulations
        nosim <- 1000
        # set seed for reproducibility
        set.seed(4321)
        # Point C
        x1 <- rnorm(n)
        x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
        x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
        # Point D
        betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
        round(apply(betas, 1, var), 5)
}
#x1      x1      x1 
#0.00110 0.00240 0.00981 
#now that x2 and x3 are correlated to x1 - the var of x1s coefficient increases
#as the correlated vars are added to the model - variance inflation


mdl <- lm(Fertility ~ . , data = swiss)
vif(mdl)
# Agriculture      Examination        Education         Catholic Infant.Mortality 
# 2.284129         3.675420         2.774943         1.937160         1.107542 
#the variance in the estimated coefficient of Education is 2.774943 times what
#it might have been if Education were not correlated with the other regressors.

mdl2 <- lm(Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss)
or
mdl2 <- lm(Fertility ~ . -Examination, data = swiss)

vif(mdl2)
# Agriculture        Education         Catholic Infant.Mortality 
# 2.147153         1.816361         1.299916         1.107528 
#the VIF for Education decreased from 2.774943 to 1.816361 - almost no effect on 
#infant.mortality - as it is not stronly correlated with the examination results

#VIF is the square of standard error inflation.

#Excluding a strongly correlated regressor might bias coefficient estimates of 
#$regressors with which it is correlated.


##Binary GLMs

download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda"
              , destfile="./data/ravensData.rda")
load("./data/ravensData.rda")
head(ravensData)

fit <- lm(ravenWinNum ~ ravenScore, data = ravensData)
summary(fit)$coef
# Estimate  Std. Error  t value   Pr(>|t|)
# (Intercept) 0.28503172 0.256643165 1.110615 0.28135043
# ravenScore  0.01589917 0.009058997 1.755069 0.09625261
#modeling the binary data linearally resulted in a bad model
#e.g. a score of 63 would lead to 63 * .0159 - resulting in greater than 1 chance
#of winning, which is not possible

#visualizing fitting logistic regression curves
x = seq(-10, 10, length = 1000)
beta0 = 0; beta1s = seq(.25, 1.5, by = .1)
beta1sNeg = beta1s * -1
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = 
     FALSE)
sapply(beta1s, function(beta1) {
        y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
        lines(x, y, type = "l", lwd = 3)
}
)
sapply(beta1sNeg, function(beta1) {
        y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
        lines(x, y, type = "l", lwd = 3)
}
)
#the negative slope flips the graph

#what if beta1 is held fixed and beta0 is varied
x = seq(-10, 10, length = 1000)
beta0s = seq(-2, 2, by = .5); beta1 = 1
plot(c(-10, 10), c(0, 1), type = "n", xlab = "X", ylab = "Probability", frame = 
     FALSE)
sapply(beta0s, function(beta0) {
        y = 1 / (1 + exp( -1 * ( beta0 + beta1 * x ) ))
        lines(x, y, type = "l", lwd = 3)
}
)
#varying the intercept shifts the curve back and forth

x = seq(-10, 10, length = 1000)
beta0 = 0
beta1 = 1
p = 1 / (1 + exp(-1 * (beta0 + beta1 * x)))
y = rbinom(prob = p, size = 1, n = length(p))

plot(x, y, frame = FALSE, xlab = "x", ylab = "y")
lines(lowess(x, y), type = "l", col = "blue", lwd = 3)
fit = glm(y ~ x, family = binomial)
lines(x, predict(fit, type = "response"), lwd = 3, col = "red")

# The plot above shows the simulated binary data (black points), the fitted logistic curve (red) and a
# lowess smoother through the data (blue). The lowess smoother shows a non-parametric estimate of
# the probability of a success at each x value. Think of it as a moving proportion. Logistic regression
# gets to move around the intercept and slope of the logistic curve to fit the data well. Here the fit says
# that the probability of a 1 for low values of x is very small, the probability of a 1 for high values of
# x is high and it is intermediate at the points in the middle.

#binary regression on the Raven's data
logRegRavens <- glm(ravenWinNum ~ ravenScore, data = ravensData, family = "binomial")
summary(logRegRavens)
# Call:
#         glm(formula = ravenWinNum ~ ravenScore, family = "binomial", 
#             data = ravensData)
# 
# Deviance Residuals: 
#         Min       1Q   Median       3Q      Max  
# -1.7575  -1.0999   0.5305   0.8060   1.4947  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)
# (Intercept) -1.68001    1.55412  -1.081     0.28
# ravenScore   0.10658    0.06674   1.597     0.11
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 24.435  on 19  degrees of freedom
# Residual deviance: 20.895  on 18  degrees of freedom
# AIC: 24.895
# 
# Number of Fisher Scoring iterations: 5

#plot the fit
plot(ravensData$ravenScore, logRegRavens$fitted, pch = 19, col = "blue", xlab = 
             "Score", ylab = "Prob Ravens Win")
#full 's' of curve not visible on plot - so exponentiate the coefs for interpretation
exp(logRegRavens$coeff)
# (Intercept)  ravenScore 
# 0.1863724   1.1124694
exp(confint(logRegRavens))
# 2.5 %   97.5 %
# (Intercept) 0.005674966 3.106384
# ravenScore  0.996229662 1.303304
# The first line of code shows that the exponentiated slope coefficient is 1.11. Thus, we estimate a 11%
# increase in the odds of winning per 1 point increase in score. However, the data are variable and
# the confident interval goes from 0.99 to 1.303. Since this interval contains 1 (or contains 0 on the log
#scale), it's not statistically significant.


#overfitting and underfitting

rm(list = ls())
library(swirl)
swirl()
Brian
6
3
11
simbias <- function(seed=8765){
        # The default seed guarantees a nice histogram. This is the only
        # reason that accepting the default, x1c <- simbias(), is required in the lesson. 
        # The effect will be evident with other seeds as well.
        set.seed(seed) 
        temp <- rnorm(100)
        # Point A
        x1 <- (temp + rnorm(100))/sqrt(2)
        x2 <- (temp + rnorm(100))/sqrt(2)
        x3 <- rnorm(100)
        # Function to simulate regression of y on 2 variables.
        f <- function(k){
                # Point B
                y <- x1 + x2 + x3 + .3*rnorm(100)
                # Point C
                c(lm(y ~ x1 + x2)$coef[2],
                  lm(y ~ x1 + x3)$coef[2])
        }
        # Point D
        sapply(1:150, f)
}

# Illustrate the effect of bogus regressors on residual squared error.
bogus <- function(){
        temp <- swiss
        # Add 41 columns of random regressors to a copy of the swiss data.
        for(n in 1:41){temp[,paste0("random",n)] <- rnorm(nrow(temp))}
        # Define a function to compute the deviance of Fertility regressed
        # on all regressors up to column n. The function, deviance(model), computes
        # the residual sum of squares of the model given as its argument.
        f <- function(n){deviance(lm(Fertility ~ ., temp[,1:n]))}
        # Apply f to data from n=6, i.e., the legitimate regressors,
        # through n=47, i.e., a full complement of bogus regressors.
        rss <- sapply(6:47, f)
        # Display result.
        plot(0:41, rss, xlab="Number of bogus regressors.", ylab="Residual squared error.",
             main="Residual Squared Error for Swiss Data\nUsing Irrelevant (Bogus) Regressors",
             pch=21, bg='red')
}

# Plot histograms illustrating bias in estimates of a regressor
# coefficient 1) when an uncorrelated regressor is missing and
# 2) when a correlated regressor is missing.
x1hist <- function(x1c){
        p1 <- hist(x1c[1,], plot=FALSE)
        p2 <- hist(x1c[2,], plot=FALSE)
        yrange <- c(0, max(p1$counts, p2$counts))
        plot(p1, col=rgb(0,0,1,1/4), xlim=range(x1c), ylim=yrange, xlab="Estimated coefficient of x1",
             main="Bias Effect of Omitted Regressor")
        plot(p2, col=rgb(1,0,0,1/4), xlim=range(x1c), ylim=yrange, add=TRUE)
        legend(1.1, 40, c("Uncorrelated regressor, x3, omitted", "Correlated regressor, x2, omitted"),
               fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
}

x1c <- simbias()
apply(x1c, 1, mean)
#x1       x1 
#1.034403 1.476944
# The actual coefficient of x1 is 1. Having been warned that omitting a correlated regressor would
# | bias estimates of x1's coefficient, we would expect the mean estimate of x1c's second row to be
# | farther from 1 than the mean of x1c's first row


# To assess significance, we should take into account that adding regressors reduces 
# residual degrees of freedom. Analysis of variance (ANOVA) is a useful way
# to quantify the significance of additional regressors.

fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
anova(fit1, fit3)
# Analysis of Variance Table
# 
# Model 1: Fertility ~ Agriculture
# Model 2: Fertility ~ Agriculture + Examination + Education
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     45 6283.1                                  
# 2     43 3180.9  2    3102.2 20.968 4.407e-07 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# The three asterisks, ***, at the lower right of the printed table indicate that 
# the null hypothesis is rejected at the 0.001 level, so at least one of the two
# additional regressors is significant.

2
# An F statistic is a ratio of two sums of squares divided by their respective degrees of freedom. If
# | the two scaled sums are independent and centrally chi-squared distributed with the same variance,
# | the statistic will have an F distribution with parameters given by the two degrees of freedom. In
# | our case, the two sums are residual sums of squares which, as we know, have mean zero hence are
# | centrally chi-squared provided the residuals themselves are normally distributed.

#calcualte the residual sum of squares - or deviance
deviance(fit3)
#[1] 3180.925 - same as in the anova function

#calculating the F value 20.968
# begin with the denominator, which is fit3's residual sum of squares
# | divided by its degrees of freedom

d <- deviance(fit3)/43

#The numerator is the difference, deviance(fit1)-deviance(fit3), divided by the difference in the
#| residual degrees of freedom of fit1 and fit3, namely 2
n <- (deviance(fit1) - deviance(fit3)) / 2
n/d
#[1] 20.96783 - same as in the anova function

#calculate the p value - which is the probability that a value of n/d or larger would be
#drawn from an F distribution which has parameters 2 and 43. - 4.407e-07

pf(n/d, 2, 43, lower.tail = FALSE)
#[1] 4.406913e-07 - same as in anova function


# Based on the calculated p-value, a false rejection of the null hypothesis is extremely unlikely. We
# | are confident that fit3 is significantly better than fit1, with one caveat: analysis of variance is
# | sensitive to its assumption that model residuals are approximately normal. If they are not, we
# | could get a small p-value for that reason. 

#testing the residuals for normal distribution - an assumption of anova
shapiro.test(fit3$residuals)

# Shapiro-Wilk normality test
# 
# data:  fit3$residuals
# W = 0.97276, p-value = 0.336

#The Shapiro-Wilk p-value of 0.336 fails to reject normality, supporting confidence in our analysis
#of variance.

#anova for nested remodels
anova(fit1, fit3, fit5, fit6)

# Analysis of Variance Table
# 
# Model 1: Fertility ~ Agriculture
# Model 2: Fertility ~ Agriculture + Examination + Education
# Model 3: Fertility ~ Agriculture + Examination + Education + Catholic
# Model 4: Fertility ~ Agriculture + Examination + Education + Catholic + 
#         Infant.Mortality
# Res.Df    RSS Df Sum of Sq       F    Pr(>F)    
# 1     45 6283.1                                   
# 2     43 3180.9  2   3102.19 30.2107 8.638e-09 ***
# 3     42 2513.8  1    667.13 12.9937 0.0008387 ***
# 4     41 2105.0  1    408.75  7.9612 0.0073357 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#each model improves on the other - decreasing p value
2
3
# Including more regressors will reduce a model's residual sum of squares, even if the new regressors
# | are irrelevant. so When adding regressors, the reduction in residual sums of squares should be tested for significance
# | above and beyond that of reducing residual degrees of freedom. R's anova() function uses an F-test
# | for this purpose.

1
1
brian_linn@calquake.com
BZ9Aui7pF68MR2nl


library(swirl)
rm(list = ls())
swirl()
Brian
delete_progress("Brian")
swirl()
Brian
3
12
#Binary Outcomes
#use glm to model a process with a binary outcome and a continuous predictor
#odds
4
#with 55%  win rate - need to offer 55 to 45 odds or 1.22222 to 1 or 11 to 9 as
#they are all the same ratio

#odds are p/(1-p)
0.55/(1-0.55)

#how raven's odds depend on the offense
4
ravenData

# A generalized linear model which has these properties supposes that the
# log odds of a win depend linearly on the score. That is, log(p/(1-p)) = b0 + b1*score. The link
# function, log(p/(1-p)), is called the logit, and the process of finding the best b0 and b1, is
# called logistic regression.
# The "best" b0 and b1 are those which maximize the likelihood of the actual win/loss record. Based
# on the score of a game, b0 and b1 give us a log odds, which we can convert to a probability, p, of
# a win. We would like p to be high for the scores of winning games, and low for the scores of
# losses.

#use glm to find the b0 and b1 to maximize likelihood of observations
mdl <- glm(ravenWinNum ~ ravenScore, family = "binomial", data = ravenData)
# The probabilities estimated by logistic regression using glm() are represented by the black curve.
# It is more reasonable than our crude estimate in several respects: It increases smoothly with
# score, it estimates that 15 points give the Ravens a 50% chance of winning, that 28 points give
# them an 80% chance, and that 55 points make a win very likely (98%) but not absolutely certain.


#model line does not predict below observed data, so it stops at 9
#use predict to find out the estimate for odds at lower scores

lodds <- predict(mdl, data.frame(ravenScore = c(0, 3, 6)))
#predict gives log odds
#so convert to probabilities

exp(lodds) / (1 + exp(lodds))
# 1         2         3 
# 0.1570943 0.2041977 0.2610505
summary(mdl)
# (Intercept) -1.68001    1.55412  -1.081     0.28
# ravenScore   0.10658    0.06674   1.597     0.11
# if b0 + b1*score estimates log odds, then exp(b0 +
# b1*score)=exp(b0)exp(b1*score) estimates odds. Thus exp(b0) is the odds of winning with a score of
# 0 (in our case 16/84,) and exp(b1) is the factor by which the odds of winning increase with every
# point scored. In our case exp(b1) = exp(0.10658) = 1.11. In other words, the odds of winning
# increase by 11% for each point scored.

exp(confint(mdl))
3
# 2.5 %   97.5 %
# (Intercept) 0.005674966 3.106384
# ravenScore  0.996229662 1.303304
#.0057 - The lower confidence bound on the odds of winning with a score of 0 is 
# near zero, which seems much more realistic than the 16/84 figure of the maximum
# likelihood model.
# the .99 suggests lower odds for more points, which is inaccurate and due to small
# sample size


3
2
anova(mdl)

# Df Deviance Resid. Df Resid. Dev
# NULL                          19     24.435
# ravenScore  1   3.5398        18     20.895

# To confidently reject this hypothesis, we would want 3.5398 to
# be larger than the 95th percentile of chi-square distribution with one degree of freedom. 

qchisq(0.95, 1)

#Count Data
rm(list = ls())
library(swirl)
swirl()
Brian
3
13
#variance of poisson = the mean of the poisson
set.seed(1234)
var(rpois(1000, 50))
# [1] 52.85013

3
#the central limit theorem implies that properly normalized sums of independent, 
#identically distributed random variables will tend to become normally 
#distributed as the number of samples grows large.
class(hits[ , 'date'])
as.integer(head(hits[, 'date']))

#use poisson regression to predict log(lambda)
mdl <- glm(visits ~ date, family = poisson, data = hits)
summary(mdl)
# Call:
#         glm(formula = visits ~ date, family = poisson, data = hits)
# 
# Deviance Residuals: 
#         Min       1Q   Median       3Q      Max  
# -5.0466  -1.5908  -0.3198   0.9128  10.6545  
# 
# Coefficients:
#         Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -3.275e+01  8.130e-01  -40.28   <2e-16 ***
#         date         2.293e-03  5.266e-05   43.55   <2e-16 ***
#         ---
#         Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 5150.0  on 730  degrees of freedom
# Residual deviance: 3121.6  on 729  degrees of freedom
# AIC: 6069.6
# 
# Number of Fisher Scoring iterations: 5

# Both coefficients are significant, being far more than two standard errors from zero.
#The Residual deviance is also very significantly less than the Null, indicating a strong effect.

#The Intercept coefficient, b0, just represents log average hits on R's Date 0, namely
#| January 1, 1970
exp(confint(mdl, 'date'))
# 2.5 %   97.5 % 
# 1.002192 1.002399 
# Visits are estimated to increase by a factor of between 1.002192 and 1.002399 per day. That is,
# | between 0.2192% and 0.2399% per day. This actually represents more than a doubling every year.

which.max(hits[,'visits'])
hits[704, ]
# date visits simplystats
# 704 2012-12-04     94          64
#Can the difference, 94-64=30 visits, be attributed to normal traffic as estimated by our model?

lambda <- mdl$fitted.values[704]
qpois(0.95, lambda)
#95% of the time we would see 33 or fewer visits, hence 30 visits would not be rare according to
#our model.

#proportion of traffic from simply statistics
mdl2 <- mdl2 <- glm(formula = simplystats ~ date, family = poisson, data = hits, offset = log(visits + 1))
qpois(.95, mdl2$fitted.values[704])

install.packages("savewav")
