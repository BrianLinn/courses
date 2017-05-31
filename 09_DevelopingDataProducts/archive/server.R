# #Apps with reactive expressions
library(shiny)
library(memisc)
library(ggplot2)
library(gridExtra)
library(caret)

#Load primary data
data(diamonds)
#Take sample of data to make plotting faster
df <- diamonds[sample(nrow(diamonds), (.5 * nrow(diamonds)), replace = FALSE, prob = NULL), ]
df$size <- df$x * df$y * df$z
diamonds$size <- diamonds$x * diamonds$y * diamonds$z

#Begin the Shiny Server Function
shinyServer(function(input, output){
        #Create training and testing data for machine learning predictions
        inTrain <- createDataPartition(y = diamonds$price, p = 0.7, list = FALSE)
        training <- diamonds[inTrain, ]
        testing <- diamonds[-inTrain, ]
        
        #Create models based on the training data
        fitColor <-     lm(price ~ carat + color,
                           data = df)
        
        fitClarity <-   lm(price ~ carat + clarity,
                           data = df)
        
        fitCut <-       lm(price ~ carat + cut,
                           data = df)
        
        fitAll <-       lm(price ~ carat + color + clarity + cut,
                           data = df)
        
        #Create predictions based on the user input
        predColor <- reactive({
                caratInput <- input$sliderCarat
                colorInput <- input$selectedColor
                predict(fitColor, newdata = data.frame(carat = caratInput,
                                                       color = colorInput))
        })
        
        predClarity <- reactive({
                caratInput <- input$sliderCarat
                clarityInput <- input$selectedClarity
                predict(fitClarity, newdata = data.frame(carat = caratInput,
                                                         clarity = clarityInput))
        })
        
        predCut <- reactive({
                caratInput <- input$sliderCarat
                cutInput <- input$selectedCut
                predict(fitCut, newdata = data.frame(carat = caratInput,
                                                       cut = cutInput))
        })
        
        predAll <- reactive({
                caratInput <- input$sliderCarat
                colorInput <- input$selectedColor
                clarityInput <- input$selectedClarity
                cutInput <- input$selectedCut
                predict(fitAll, newdata = data.frame(carat = caratInput,
                                                       color = colorInput,
                                                       clarity = clarityInput,
                                                       cut = cutInput))
        })
        output$cutPricePlot <- renderPlot({
                colorInput <- input$selectedPointColor
                gCut <- ggplot(diamonds, aes(x = cut, y = (price/carat)))
                if(input$selectedPointColor == "Color"){
                        gCut <- gCut + geom_point(aes(size = carat,
                                       colour = factor(color))) +
                                scale_colour_discrete(name  = "Color") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Cut"){
                        gCut <- gCut + geom_point(aes(size = carat,
                                                      colour = factor(cut))) +
                                scale_colour_discrete(name  = "Cut") +
                                scale_size_continuous(name  = "Carat")
                }       
                if(input$selectedPointColor == "Clarity"){
                        gCut <- gCut + geom_point(aes(size = carat,
                                                      colour = factor(clarity))) +
                                scale_colour_discrete(name  = "Clarity") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Size"){
                        gCut <- gCut + geom_point(aes(size = carat,
                                                      color = size)) +
                                scale_colour_continuous(name  = "Size") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Carat"){
                        gCut <- gCut + geom_point(aes(size = carat,
                                                      color = carat)) +
                                scale_colour_continuous(name  = "Carat") +
                                scale_size_continuous(name  = "Carat")
                }
                gCut <- gCut +
                        xlab("Cut") +
                        ylab("Price per Carat") 
                gCut
        })
        output$clarityPricePlot <- renderPlot({
                colorInput <- input$selectedPointColor
                gClarity <- ggplot(diamonds, aes(x = clarity, y = (price/carat)))
                if(input$selectedPointColor == "Color"){
                        gClarity <- gClarity + geom_point(aes(size = carat,
                                                              colour = factor(color))) +
                                scale_colour_discrete(name  = "Color") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Cut"){
                        gClarity <- gClarity + geom_point(aes(size = carat,
                                                              colour = factor(cut))) +
                                scale_colour_discrete(name  = "Cut") +
                                scale_size_continuous(name  = "Carat")
                }       
                if(input$selectedPointColor == "Clarity"){
                        gClarity <- gClarity + geom_point(aes(size = carat,
                                                              colour = factor(clarity))) +
                                scale_colour_discrete(name  = "Clarity") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Size"){
                        gClarity <- gClarity + geom_point(aes(size = carat,
                                                              color = size)) +
                                scale_colour_continuous(name  = "Size") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Carat"){
                        gClarity <- gClarity + geom_point(aes(size = carat,
                                                              color = carat)) +
                                scale_colour_continuous(name  = "Carat") +
                                scale_size_continuous(name  = "Carat")
                }
                gClarity <- gClarity +
                        xlab("Clarity") +
                        ylab("Price per Carat") 
                gClarity
        })
        output$sizePricePlot <- renderPlot({
                colorInput <- input$selectedPointColor
                gSize <- ggplot(diamonds, aes(x = size, y = (price/carat)))
                if(input$selectedPointColor == "Color"){
                        gSize <- gSize + geom_point(aes(size = carat,
                                                        colour = factor(color))) +
                                scale_colour_discrete(name  = "Color") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Cut"){
                        gSize <- gSize + geom_point(aes(size = carat,
                                                        colour = factor(cut))) +
                                scale_colour_discrete(name  = "Cut") +
                                scale_size_continuous(name  = "Carat")
                }       
                if(input$selectedPointColor == "Clarity"){
                        gSize <- gSize + geom_point(aes(size = carat,
                                                        colour = factor(clarity))) +
                                scale_colour_discrete(name  = "Clarity") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Size"){
                        gSize <- gSize + geom_point(aes(size = carat,
                                                        color = size)) +
                                scale_colour_continuous(name  = "Size") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Carat"){
                        gSize <- gSize + geom_point(aes(size = carat,
                                                        color = carat)) +
                                scale_colour_continuous(name  = "Carat") +
                                scale_size_continuous(name  = "Carat")
                }
                gSize <- gSize +
                        xlab("Size") +
                        ylab("Price per Carat") 
                gSize
        })
        output$colorPricePlot <- renderPlot({
                colorInput <- input$selectedPointColor
                gColor <- ggplot(diamonds, aes(x = color, y = (price/carat)))
                if(input$selectedPointColor == "Color"){
                        gColor <- gColor + geom_point(aes(size = carat,
                                                      colour = factor(color))) +
                                scale_colour_discrete(name  = "Color") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Cut"){
                        gColor <- gColor + geom_point(aes(size = carat,
                                                      colour = factor(cut))) +
                                scale_colour_discrete(name  = "Cut") +
                                scale_size_continuous(name  = "Carat")
                }       
                if(input$selectedPointColor == "Clarity"){
                        gColor <- gColor + geom_point(aes(size = carat,
                                                      colour = factor(clarity))) +
                                scale_colour_discrete(name  = "Clarity") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Size"){
                        gColor <- gColor + geom_point(aes(size = carat,
                                                      color = size)) +
                                scale_colour_continuous(name  = "Size") +
                                scale_size_continuous(name  = "Carat")
                }
                if(input$selectedPointColor == "Carat"){
                        gColor <- gColor + geom_point(aes(size = carat,
                                                      color = carat)) +
                                scale_colour_continuous(name  = "Carat") +
                                scale_size_continuous(name  = "Carat")
                }
                gColor <- gColor +
                        xlab("Color") +
                        ylab("Price per Carat") 
                gColor
        })
        output$scatterPlot <- renderPlot({
                caratInput <- input$sliderCarat
                colorInput <- input$selectedColor
                cutInput <- input$selectedCut
                clarityInput <- input$selectedClarity
                #points(caratInput, predColor(), col = "red", pch = 16, cex = 2)
                # points(caratInput, predClarity(), col = "blue", pch = 16, cex = 2)
                g <- ggplot(df, aes(x = carat, y = price))
                if(input$selectedPointColor == "Color"){
                        g <- g + geom_point(aes(colour = factor(color)))
                }
                if(input$selectedPointColor == "Cut"){
                        g <- g + geom_point(aes(colour = factor(cut)))
                }
                if(input$selectedPointColor == "Clarity"){
                        g <- g + geom_point(aes(colour = factor(clarity)))
                }
                if(input$selectedPointColor == "Size"){
                        g <- g + geom_point(aes(colour = size))
                }
                if(input$selectedPointColor == "Carat"){
                        g <- g + geom_point(aes(colour = carat))
                }
                g <- g + 
                        xlab("Carats") +
                        ylab("Price") +
                        scale_y_continuous(limits = c(0, max(range(df$price)))) +
                        xlim(c(0, 3.5))
                if(input$selectedPointColor == "Color"){
                        g <- g + labs(colour = "Color")
                }
                if(input$selectedPointColor == "Cut"){
                        g <- g + labs(colour = "Cut")
                }
                if(input$selectedPointColor == "Clarity"){
                        g <- g + labs(colour = "Clarity")
                }
                if(input$selectedPointColor == "Size"){
                        g <- g + labs(colour = "Size")
                }
                if(input$selectedPointColor == "Carat"){
                        g <- g + labs(colour = "Carat")
                }
                g <- g + ggtitle("Price ~ Carats")
                if(input$showFittedLine == TRUE){
                        g <- g + geom_smooth(method = "loess", se = FALSE)
                }
                g
        })
        
        output$predColor <- renderText({
                predColor()
        })
        
        output$predClarity <- renderText({
                predClarity()
        })
        
        output$predCut <- renderText({
                predCut()
        })
        
        output$predSize <- renderText({
                predSize()
        })
        
        output$predAll <- renderText({
                predAll()
        })
        
})

?geom_point
