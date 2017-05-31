#Class -> Object -> Method -> Generic Function


class(1)
class(TRUE)
class(rnorm(100))
class(NA)
class("foo")
x <- rnorm(100)
y <- x + rnorm(100)
fit <- lm(y ~ x)
class(fit)

#generic functions
# The generic function checks the class of the object.
# A search is done to see if there is an appropriate method for that class.
# If there exists a method for that class, then that method is called on the object and we’re done.
# If a method for that class does not exist, a search is done to see if there is a default method for the generic. If a default exists, then the default method is called.
# If a default method doesn’t exist, then an error is thrown.

print
mean
#find the appropriate method for the passed data type

methods("mean")

#s4 version of print
show
showMethods("show")


set.seed(2)
x <- rnorm(100)
mean(x)
getS3method("mean", "default")

set.seed(3)
df <- data.frame(x = rnorm(100), y = 1:100)
sapply(df, mean)
# The class of df is "data.frame"; each column can be an object of a different class
# We sapply over the columns and call the mean function
# In each column, mean checks the class of the object and dispatches the appropriate method.
# We have a numeric column and an integer column; mean calls the default method for both

set.seed(10)
x <- rnorm(100)
plot(x) #scatterplot
x <- as.ts(x)
plot(x) #linegraph

#Creating a class
setClass()
# At a minimum you need to specify the name of the class
# You can also specify data elements that are called slots
# You can then define methods for the class with the setMethod function 
# Information about a class definition can be obtained with the showClass function
library(methods)
setClass("polygon",
         representation( x = "numeric",
                         y = "numeric"))
#now a method to plot the ploygon class
# For setMethod you need to specify a generic function (plot), and a signature.
# A signature is a character vector indicating the classes of objects that are 
#accepted by the method. 
# In this case, the plot method will take one type of object, a polygon object.
setMethod("plot", "polygon",
           function(x, y, ...) {
                   plot(x@x, x@y, type = "n", ...)
                   xp <- c(x@x, x@x[1])
                   yp <- c(x@y, x@y[1])
                   lines(xp, yp)
           })
showMethods(plot)
p <- new("polygon", x = c(1, 2, 3, 4), y = c(1, 2, 3, 1))
p
plot(p)

# #Summary
# Developing classes and associated methods is a powerful way to extend the functionality of R
# Classes define new data types
# Methods extend generic functions to specificy the behavior of generic functions
# on new classes
# As new data types and concepts are created, classes/methods provide a way for
#you to develop an intuitive interface to those data/concepts for users


#Quiz
#1 - explicit software license
#2
install.packages("pryr")
library(pryr)
ftype(colSums)
ftype(predict)
ftype(dgamma)
showMethods(show)
getMethod(show)


library(plotly)
f <- list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
)
x <- list(
        title = "STEVREGSDF",
        titlefont = f
)
y <- list(
        title = "y Axis",
        titlefont = f
)
p <- plot_ly(x = rnorm(10), y = rnorm(10), mode = "markers") %>%
        layout(xaxis = x, yaxis = y)
p
