library(UsingR)
library(reshape)
library(reshape2)
library(ggplot2)
library(manipulate)
data(galton)
long <- melt(galton)


ggplot(galton, aes(x = parent, y = child)) + geom_point()

names(long)
g <- ggplot(long, aes(x = value, fill = variable))
g <- g +
        geom_histogram(col = "black", binwidth = 1)
g <- g + facet_grid(. ~ variable)
g

#least squares

myHist <- function(mu) {
        mse <- mean((galton$child - mu)^2)
        g <- ggplot(galton, aes(x = child)) +
                geom_histogram(fill = "salmon", col = "black", binwidth = 1)
        g <- g + geom_vline(xintercept = mu, size = 3)
        g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
        g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

g <- ggplot(galton, aes(x = child)) +
        geom_histogram(fill = "salmon", col = "black", binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 3)
g

ggplot(galton, aes(x = parent, y = child)) + geom_point()



freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, 
     xlab = "parent", ylab = "child")


myPlot <- function(beta){
        y <- galton$child - mean(galton$child)
        x <- galton$parent - mean(galton$parent)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("child", "parent", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))

?lm

lm(I(child - mean(child))~ I(parent - mean(parent)) - 1, data = galton)


x = c(0.725, 0.429, -0.372, 0.863)
mnx <- mean(x)
sum(x -mnx)^2
mnx
w <- c(2, 2, 1, 1)

sum(w * x)/sum(w)


lm(I(parent - mean(parent)) ~ I(child - mean(child)) - 1, data = galton)


myPlot <- function(beta){
        y <- galton$child - mean(galton$parent)
        x <- galton$parent - mean(galton$child)
        freqData <- as.data.frame(table(x, y))
        names(freqData) <- c("parent", "child", "freq")
        plot(
                as.numeric(as.vector(freqData$parent)), 
                as.numeric(as.vector(freqData$child)),
                pch = 21, col = "black", bg = "lightblue",
                cex = .15 * freqData$freq, 
                xlab = "parent", 
                ylab = "child"
        )
        abline(0, beta, lwd = 3)
        points(0, 0, cex = 2, pch = 19)
        mse <- mean( (y - beta * x)^2 )
        title(paste("beta = ", beta, "mse = ", round(mse, 3)))
}
manipulate(myPlot(beta), beta = slider(0.6, 1.2, step = 0.02))


y <- galton$parent
x <- galton$child
yc = y - mean(y)
xc = x - mean(x)
sum(yc * xc)/ sum(xc^2)
mean(yc)
mean(xc)

#Notation
data(galton)
summary(galton)
cor(galton$child, galton$parent)
cor(galton)


y <- galton$child
x <- galton$parent

xc <- x - mean(x, na.rm = TRUE)
yx <- y - mean(x, na.rm = TRUE)

xs <- x/sd(x)
sd(xs)

xz <- (x - mean(x))/sd(x)
yz <- (y - mean(y))/sd(y)
cor(xz, yz)


#ordinary least saquares
#first use linear regression to fit galton's data
y <- galton$child
x <- galton$parent
beta1 <- cor(y, x) *  sd(y) / sd(x) #slope
beta0 <- mean(y) - beta1 * mean(x) #intercept
rbind(c(beta0, beta1), coef(lm(y ~ x))) 
#bind intercept, slope to liner model of 
#child as predicted by parent

# Next Regression through the origin yields the same slope slope 
#if you center the data first
yc <- y - mean(y)
xc <- x - mean(x)
beta12 <- sum(yc * xc) / sum(xc ^ 2)
c(beta12, coef(lm(y ~ x))[2])
#results matches the 'x' result from the fitted linear model


# Next if we Normalize the variables the slope matches the 'intercept' froim the lm
yn <- (y - mean(y))/sd(y)
xn <- (x - mean(x))/sd(x)
c(cor(y, x), cor(yn, xn), coef(lm(yn ~ xn))[2])


abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), 
       sd(y) / sd(x) * cor(y, x), 
       lwd = 3, col = "red")
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), 
       sd(y) cor(y, x) / sd(x), 
       lwd = 3, col = "blue")
abline(mean(y) - mean(x) * sd(y) / sd(x), 
       sd(y) / sd(x), 
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19)

#plot all the data with circle size
library(UsingR)
data(galton)
library(dplyr); library(ggplot2)
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g

replot but add the lm line with CI bars
g <- ggplot(filter(freqData, freq > 0), aes(x = parent, y = child))
g <- g  + scale_size(range = c(2, 20), guide = "none" )
g <- g + geom_point(colour="grey50", aes(size = freq+20, show_guide = FALSE))
g <- g + geom_point(aes(colour=freq, size = freq))
g <- g + scale_colour_gradient(low = "lightblue", high="white")  
g <- g + geom_smooth(method="lm", formula=y~x)
g


#exercises
#1
data(father.son)
s <- father.son$sheight
f <- father.son$fheight
fit <- lm(s ~ f)
plot(s, f)
abline(fit)

#center vars from 1
sc <- s - mean(s)
fc <- f - mean(f)
summary(fit)
fitc <- lm(sc ~ fc)
fitc
#or
sum(fc * sc)/sum(fc^2)
#sum(predictor * outcome)/sum(predictor^2)

#now normalize and show that the slope matches the corr
y = father.son$sheight
x = father.son$fheight
sfcorr <- cor(s, f)
cor(x, y)
cor(s, f)
sfcorr
sz <- (s - mean(s))/sd(s)
fz <- (f - mean(f))/sd(f)
fitZ <- lm(sz ~ fz)
fitZ
#manually calculate the slope
sum(fz * sz)/sum((fz^2))

#4
fhs <- 63
fit <- lm(s ~ f)
summary(fit)
predict(fit, newdata = data.frame(f = 63))
coef(fit)
#manually
beta0 = coef(fit)[1]
beta1 = coef(fit)[2]
beta0 + beta1 * 63


#Here are the vectors of variations or tweaks
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #one for the slope
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #one for the intercept
lhs <- numeric()
rhs <- numeric()
#left side of eqn is the sum of squares of residuals of the tweaked regression line
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#right side of eqn is the sum of squares of original residuals + sum of squares of two tweaks
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)

lhs - rhs
all.equal(lhs, rhs)
varChild <- var(galton$child)
varRes <- var(fit$residuals)
varEst <- var(est(ols.slope, ols.ic))
all.equal(varChild, varRes + varEst)
2

#earthquake data
efit <- lm(accel ~ mag+dist, attenu)
#verify the mean is 0
mean(efit$residuals)
#Using the R function cov verify the residuals are uncorrelated with the magnitude predictor, attenu$mag
cov(efit$residuals, attenu$mag)
cov(efit$residuals, attenu$dist)

rm(list = ls())
#Regression to the mean
library(UsingR)
data(father.son)
#normalized data is centered - subtract the mean, an dscaled - divided by sd
yz <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
xz <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(xz, yz)
myPlot <- function(x, y) {
        plot(x, y, 
             xlab = "Father's height, normalized",
             ylab = "Son's height, normalized",
             xlim = c(-3, 3), ylim = c(-3, 3),
             bg = "lightblue", col = "black", cex = 1.1, pch = 21, 
             frame = FALSE)
}

## Plot the data, code
myPlot(xz, yz)
abline(0, 1) # if there were perfect correlation
abline(0, rho, lwd = 2) # father predicts son
abline(0, 1 / rho, lwd = 2) # son predicts father, son on vertical axis
abline(h = 0); abline(v = 0) # reference lines for no relathionship


#plot the data
library(ggplot2)
g = ggplot(data.frame(xz, yz), aes(x = xz, y = yz))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g
