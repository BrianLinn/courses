rm(list = ls())

library(ggplot2)
library(gridExtra)
library(caret)

set.seed(12345)
data(diamonds)
df <- diamonds[sample(nrow(diamonds), (.5 * nrow(diamonds)), replace = FALSE, prob = NULL), ]
df$xyz <- df$x * df$y * df$z
# The actual price distribution is skewed, and the log distribution is 
#more Gaussian, so we will use the log price to plot

# There is a clear linear relationship between the carat size and the diamond price
# Using the square root of the carat to ease interpretation
# Add a second predictor to see if any is more accurate

gCarat <-       ggplot(df, aes(x = carat, y = price)) +
                #geom_point(aes(colour = factor(color))) +
                #geom_point(aes(colour = factor(cut))) +
                #geom_point(aes(colour = factor(clarity))) +
                geom_point(aes(colour = factor(xyz))) +
                xlab("Carats") +
                ylab("Price") +
                scale_y_continuous(limits = c(0, max(range(df$price)))) +
                xlim(c(0, 3.5)) +
                #labs(colour = "Color") +
                geom_smooth(method = "loess", se = FALSE) +
                ggtitle("Price ~ Carats")
gCarat



grid.arrange(gCut, gColor, gClarity, gCarat, ncol = 4)


# A diamond to test
##       carat       cut color clarity depth table price    x    y    z
## 13696     1 Very Good     G     VS2  61.8    59  5600 6.29 6.37 3.91
testColor <- data.frame(carat = 1, color = 'G')
testClarity <- data.frame(carat = 1, clarity ='VS2')
testCut <- data.frame(carat = 1, cut = 'Very Good')
testDiamond <- data.frame(carat = 1, cut = 'Very Good', color = 'G', 
                          clarity = 'VS2', x = 6.29, y = 6.37, z = 3.91)


#Create testing and training data
inTrain <- createDataPartition(y = diamonds$price, p = 0.7, list = FALSE)
training <- diamonds[inTrain, ]
testing <- diamonds[-inTrain, ]

fitColor2 <- train(price ~ carat + color,
                   data = training,
                   method = "lm",
                   trControl = trainControl(method='cv', number = 2))


fitClarity2 <- train(price ~ carat + clarity,
                     data = training,
                     method = "lm",
                     trControl = trainControl(method='cv', number = 2))


fitCut2 <- train(price ~ carat + cut,
                 data = training,
                 method = "lm",
                 trControl = trainControl(method='cv', number = 2))

fitDiamond2 <- train(price ~ carat + color + clarity + cut + x + y + z,
                     data = training,
                     method = "lm",
                     trControl = trainControl(method='cv', number = 2))

predColor2 <- predict(fitColor2, newdata = testColor, interval = "prediction",
                      level = 0.95)

predClarity2 <- predict(fitClarity2, newdata = testClarity, interval = "prediction",
                        level = 0.95)

predCut2 <- predict(fitCut2, newdata = testCut, interval = "prediction", level = 0.95)

predDiamond2 <- predict(fitDiamond2, newdata = testDiamond, 
                        interval = "prediction", level = 0.95)

predColor2
predClarity2
predCut2
predDiamond2

# Price by carat
plotCarat <-    ggplot(diamonds, aes(x = carat, y = log(price))) +
        geom_point(aes(colour = price)) +
        xlab("Carats") +
        ylab("Price") +
        scale_y_continuous(limits = c(5.5, 10)) +
        labs(colour = "Price") +
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Model 1: Price ~ Carats")
plotCarat

#Price by color
plotColor <-    ggplot(diamonds, aes(x = color, y = log(price))) +
        geom_point(aes(colour = price)) +
        xlab("Color") +
        ylab("Price") +
        scale_y_continuous(limits = c(5.5, 10)) +
        labs(colour = "Price") +
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Price by Color")
plotColor

#Price by Cut
plotCut <-      ggplot(diamonds, aes(x = cut, y = log(price))) +
        geom_point(aes(colour = price)) +
        xlab("Cut") +
        ylab("Price") +
        scale_y_continuous(limits = c(5.5, 10)) +
        labs(colour = "Price") +
        geom_smooth(method = "lm", se = FALSE) +
        ggtitle("Price by Cut")
plotCut

unique(diamonds$cut)
unique(diamonds$color)
unique(diamonds$clarity)


range(df$xyz)
quantile(df$xyz)
?diamonds
?cut
pdf <- cut(df$size, 5, 
           labels = c("Diminutive", "Small", "Medium", "Large", "Gigantic"))
pdf
unique(pdf)
as.numeric(df$sizeFactor)
diamonds$size
diamonds$sizeFactor
fitSize <-      train(price ~ carat + size,
                      data = training,
                      method = "lm",
                      trControl = trainControl(method='cv', number = 2))
sz <- df$sizeFactor
as.numeric(sz)
unique(as.numeric(sz))

predSize <- predict(fitSize, newdata = data.frame(carat = 2.1,
                                              size = 5))
predSize
as.numeric(df$sizeFactor)

fitCut <- lm(price ~ carat + cut,
             data = df)

fitCutLearn <-       train(price ~ carat + cut,
                      data = training,
                      method = "lm",
                      trControl = trainControl(method='cv', number = 2))

predCut <- predict(fitCut, newdata = data.frame(carat = 2,
                                             cut = "Good"))

predCutLearn <- predict(fitCut, newdata = testing)

predCutLearnResults <- data.frame(Observations = df$price, 
                                  Predicted = fitted(fitCut))


g <-    ggplot(df, aes(x = carat, y = price)) +
        geom_point(aes(colour = factor(color))) +
        xlab("Carats") +
        ylab("Price") +
        scale_y_continuous(limits = c(0, max(range(df$price)))) +
        xlim(c(0, 3.5))+
        labs(colour = "Color")+ 
        geom_smooth(method = "loess", se = FALSE)

g <-    g + 
        geom_point(aes(x = caratinput, y = predCut),colour="red",size=2)

caratinput <- 2

g
head(predCut2)
summary(predCut2)

gCut <- ggplot(predCutLearnResults, aes(x = Observations, y = Predicted)) +
        geom_point()
gCut
str(predCut)
summary(predCut)
summary(fitCut)

data("diamonds")
gCut <- ggplot(diamonds, aes(x = cut, y = (price/carat)))
gCut <- gCut +
        geom_point(aes(size = carat,
                       color = carat)) +
        xlab("Cut") +
        ylab("Price per Carat") 
gCut

gCut <- ggplot(diamonds, aes(x = cut, y = (price/carat)))
gCut <- gCut +
        geom_point(aes(size = carat,
                       color = cut)) +
        xlab("Cut") +
        ylab("Price per Carat") 
gCut

qplot(x = cut, y = (price/carat), data = diamonds,
      xlab = "Cut",
      ylab = "Price per Carat",
      colour = carat)

qplot(x = color, y = (price/carat), data = diamonds,
      xlab = "Color",
      ylab = "Price per Carat",
      colour = carat)

qplot(x = clarity, y = (price/carat), data = diamonds,
      xlab = "Clarity",
      ylab = "Price per Carat",
      colour = carat)


gCut <- ggplot(diamonds, aes(x = cut, y = (price/carat)))
gCut <- gCut + geom_point(aes(size = carat,
                              colour = factor(color))) +
        scale_colour_discrete(name  = "Color") +
        scale_size_continuous(name  = "Carat")
gCut


install.packages("rsconnect")
