#1
rm(list = ls())
x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)

sum(w * x)/sum(w)


#2
#x - regressor, y - outcome
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

#subtract 1 from the regressor to fit through the origin
lm(y ~ x - 1)

#3
data(mtcars)
lm(mtcars$mpg ~ mtcars$wt)

#4
.5*2

#5
0.4*1.5

#6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xmn <- mean(x)
xsd <- sd(x)

xz <- (x - xmn)/xsd
xz[1]


#7
#x is predictor and y is outcome
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
lm(y ~ x)

beta1 <- cor(x,y)*sd(y)/sd(x)
beta0 <- mean(y) - beta1 * mean(x)
beta0

#8
#if predictor and response have mean 0  = what is intercept for fitted lm
# identical - 0

#9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)
