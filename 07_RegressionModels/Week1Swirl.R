library(swirl)
swirl()
Brian
4
install_from_swirl("Regression Models")
3
3
#plot the children hieghts(outcome) as a function of parent heights(predictor)
plot(child ~ parent, galton)
#add jitter to the plot to show some separation of like measurements
plot(jitter(child, factor = 4) ~ parent, galton)

#plot the regression line
regrline <- lm(child ~ parent, galton)
#add the regression line to the plot
abline(regrline, lwd = 3, col = 'red')

summary(regrline)


#residuals
#first fit the model
fit <- lm(child ~ parent, data = galton)
summary(fit)
mean(fit$residuals)
cov(fit$residuals, galton$parent)
2
ols.ic <- fit$coef[1]
ols.slope <- fit$coef[2]

#ordinary least squares
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
manipulate(myPlot(beta), beta = manipulate::slider(0.4, .8, step = 0.02))
4


cor(gpa_nor, gch_nor)
3
#use parents height as predictor - the independent variable
#use childs height as predictoed - the outcome or dependent variable
l_nor <- lm(gch_nor ~ gpa_nor)
2
#the slope of the lm line is the correlatio nof the 2 data sets

3
#if we made parents dependent on children the correlation would be cor(x, y) 8 sd(x)/sd(y)

#plot the original Galton data points with larger dots for more freq pts
y <- galton$child
x <- galton$parent
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .07 * freqData$freq, xlab = "parent", ylab = "child")

#original regression line, children as outcome, parents as predictor
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x) * cor(y, x),  #slope
       lwd = 3, col = "red")

#new regression line, parents as outcome, children as predictor
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercept
       sd(y) / cor(y, x) / sd(x), #slope
       lwd = 3, col = "blue")

#assume correlation is 1 so slope is ratio of std deviations
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercept
       sd(y) / sd(x),  #slope
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #big point of intersection