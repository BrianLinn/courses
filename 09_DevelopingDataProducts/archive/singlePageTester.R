# rm(list = ls())
# 
# library(ggplot2)
# library(gridExtra)
# library(caret)
# 
# data(diamonds)
# 
# 
# # The actual price distribution is skewed, and the log distribution is 
# #more Gaussian, so we will use the log price to plot
# 
# # There is a clear linear relationship between the carat size and the diamond price
# # Using the square root of the carat to ease interpretation
# # Add a second predictor to see if any is more accurate
# 
# gLogColor <-    ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
#                 geom_point(aes(colour = factor(color))) +
#                 xlab("Carats") +
#                 ylab("Price") +
#                 scale_y_continuous(limits = c(5.5, 10)) +
#                 labs(colour = "Color") +
#                 geom_smooth(method = "lm", se = FALSE) +
#                 ggtitle("Model 1: Price ~ Carats")
# 
# 
# gLogClarity <-  ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
#                 geom_point(aes(colour = factor(clarity))) +
#                 xlab("Carats") +
#                 ylab("Price") +
#                 scale_y_continuous(limits = c(5.5, 10)) +
#                 labs(colour = "Clarity") +
#                 geom_smooth(method = "lm", se = FALSE) +
#                 ggtitle("Model 2: Price ~ Carats + Clarity")
# 
# gLogCut <-      ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
#                 geom_point(aes(colour = cut)) +
#                 xlab("Carats") +
#                 ylab("Price") +
#                 scale_y_continuous(limits = c(5.5, 10)) +
#                 labs(colour = "Cut") +
#                 geom_smooth(method = "lm", se = FALSE) +
#                 ggtitle("Model 3: Price ~ Carats + Cut")
# 
# gLogDiamond <-  ggplot(diamonds, aes(x = carat, y = log(price))) +
#         geom_point(aes(colour = price)) +
#         xlab("Carats") +
#         ylab("Price") +
#         scale_y_continuous(limits = c(5.5, 10)) +
#         geom_smooth(method = "lm", se = FALSE) +
#         ggtitle("Model 4: Price ~ <Many Predictors>")
# 
# grid.arrange(gLogColor, gLogClarity, gLogCut, gLogDiamond, ncol = 4)
# 
# 
# # A diamond to test
# ##       carat       cut color clarity depth table price    x    y    z
# ## 13696     1 Very Good     G     VS2  61.8    59  5600 6.29 6.37 3.91
# testColor <- data.frame(carat = 1, color = 'G')
# testClarity <- data.frame(carat = 1, clarity ='VS2')
# testCut <- data.frame(carat = 1, cut = 'Very Good')
# testDiamond <- data.frame(carat = 1, cut = 'Very Good', color = 'G', 
#                           clarity = 'VS2', x = 6.29, y = 6.37, z = 3.91)
# 
# 
# #Create testing and training data
# inTrain <- createDataPartition(y = diamonds$price, p = 0.7, list = FALSE)
# training <- diamonds[inTrain, ]
# testing <- diamonds[-inTrain, ]
# 
# fitColor2 <- train(price ~ carat + color,
#                    data = training,
#                    method = "lm",
#                    trControl = trainControl(method='cv', number = 2))
# 
# 
# fitClarity2 <- train(price ~ carat + clarity,
#                    data = training,
#                    method = "lm",
#                    trControl = trainControl(method='cv', number = 2))
# 
# 
# fitCut2 <- train(price ~ carat + cut,
#                    data = training,
#                    method = "lm",
#                    trControl = trainControl(method='cv', number = 2))
# 
# fitDiamond2 <- train(price ~ carat + color + clarity + cut + x + y + z,
#                    data = training,
#                    method = "lm",
#                    trControl = trainControl(method='cv', number = 2))
# 
# predColor2 <- predict(fitColor2, newdata = testColor, interval = "prediction",
#                      level = 0.95)
# 
# predClarity2 <- predict(fitClarity2, newdata = testClarity, interval = "prediction",
#                        level = 0.95)
# 
# predCut2 <- predict(fitCut2, newdata = testCut, interval = "prediction", level = 0.95)
# 
# predDiamond2 <- predict(fitDiamond2, newdata = testDiamond, 
#                        interval = "prediction", level = 0.95)
# 
# predColor2
# predClarity2
# predCut2
# predDiamond2
# 
# 
# # # Now fit a linear model for each graphed relationship
# # fitColor <- lm(price ~ carat + color, data = diamonds)
# # fitClarity <- lm(price ~ carat + clarity, data = diamonds)
# # fitCut <- lm(price ~ carat + cut, data = diamonds)
# # fitDiamond <- lm(price ~ carat + color + clarity + cut + color + x + y + z, data = diamonds)
# 
# 
# # predColor <- predict(fitColor, newdata = testColor, interval = "prediction",
# #                      level = 0.95)
# # 
# # predClarity <- predict(fitClarity, newdata = testClarity, interval = "prediction",
# #                        level = 0.95)
# # 
# # predCut <- predict(fitCut, newdata = testCut, interval = "prediction", level = 0.95)
# # 
# # predDiamond <- predict(fitDiamond, newdata = testDiamond, 
# #                        interval = "prediction", level = 0.95)
# # 
# # predColor
# # predClarity
# # predCut
# # predDiamond
# 
# # 
# # gLogX <- ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
# #          geom_point(aes(colour = x)) +
# #         ggtitle("Model 2: Price ~ Carats + X")
# # gLogX
# # 
# # gLogY <- ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
# #          geom_point(aes(colour = y)) +
# #          ggtitle("Model 2: Price ~ Carats + Y")
# # gLogY
# # 
# # gLogZ <- ggplot(diamonds, aes(x = carat^(1/2), y = log(price))) +
# #          geom_point(aes(colour = z)) +
# #          ggtitle("Model 2: Price ~ Carats + Z")
# # gLogZ
# 
# 
# 
# 
# head(diamonds)
# plot(x = diamonds$carat, y = diamonds$price, col = diamonds$clarity)
# str(diamonds)
# warnings()
# #Plot other predictors as color to see if those impact price
# names(diamonds)
# 
# lm1 <- lm(price ~ carat, data = diamonds)
# lm2 <- update(lm1,~ . + carat)
# lm3 <- update(lm2,~ . + cut)
# lm4 <- update(lm3,~ . + color)
# lm5 <- update(lm4,~ . + clarity)
# mtable(lm1,lm2,lm3,lm4,lm5)
# 
# abline(lm1, col = "red", lwd = 2)
# abline(lm2, col = "green", lwd = 2)
# abline(lm3, col = "purple", lwd = 2)
# 
# 
# lm1$coeff
# lm1$fitted.values
# lm1$fitted
# lm1$coefficients
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# rm(list = ls())
# 
# library(shiny)
# library(shinythemes)
# library(plotly)
# library(ggplot2)
# 
# #### server
# server <- function(input, output, session) {
#         output$my_output_UI <- renderUI({
#                 list(
#                         h4(style = "color:blue;", "My selection list"),
#                         selectInput(inputId = "myselect", label = "", 
#                                     choices = selections)
#                 )
#         })
#         
#         selections <- c("New York", "Flipadelphia")
#         
#         observeEvent(input$button1, {
#                 selections <<- c(input$text1, selections)
#                 updateSelectInput(session, "myselect", choices = selections, 
#                                   selected = selections[1])
#         })
#         
#         output$my_output_text1 <- renderText({
#                 init <- "Your value is: "
#                 return(paste0(init, input$text1))
#         })
#         
#         output$my_output_plot <- renderPlot({
#                 plot(1:10, 1:10, pch = 16, col = 1:10, cex = 1:10, 
#                      main = input$text1)
#         })
#         
#         output$my_output_text <- renderText({
#                 return(input$text1)
#         })
#         
#         myresults <- reactive({
#                 paste(input$text1, input$slider1)
#         })
#         
#         myresults_lim <- eventReactive(input$text1, {
#                 paste(input$text1, input$slider1)
#         })
#         
#         observe(updateTextInput(session, "myresults", value = myresults()))
#         observe(updateTextInput(session, "myresults_lim", value = myresults_lim()))
#         
#         
# }
# 
# 
# #### ui
# ui <- fluidPage(
#         includeCSS("C:/Users/blinn/Documents/R/courses/09_DevelopingDataProducts/ddpCourseProjectApp/www/style.css"),
# 
#         theme = shinytheme("cosmo"),
# 
#         titlePanel("Using the cosmo theme"),
# 
#         sidebarLayout(
# 
#                 sidebarPanel(
#                         h3("This is the Sidebar"),
#                         actionButton("button1", "Click to Add Selections"),
#                         # a slider
#                         sliderInput(inputId = "slider1", label = "Slide it",
#                                     min = 10, max = 100, value = 50),
#                         # text input
#                         textInput(inputId = "text1", label = "Input new selections here")
#                 ), #end sidebarpanel
# 
#                 mainPanel(
#                         tabsetPanel(
#                                 tabPanel("Plot", value = "plotTab",
#                                          textInput("myresults",
#                                                    "Text box + slider when either changes",
#                                                    "Initial Value"),
#                                          textInput("myresults_lim",
#                                                    "Text box + slider when text box changes",
#                                                    "Initial Value"),
#                                          textOutput("my_output_text"),
#                                          textOutput("my_output_text1"),
#                                          plotOutput("my_output_plot")),
#                                 tabPanel("Summary", value = "summaryTab",
#                                          uiOutput("my_output_UI")),
#                                 tabPanel("Table", value = "tableTab")
#                         ) #end tabsetpanel
#                 ) #end mainpanel
#         )# end sidebar layout
# )
# 
# 
# #### call the app
# shinyApp(ui = ui, server = server) # to launch the app
# ?actionButton
# ?tabPanel
# 
# remove(list = ls(all.names = TRUE))
# library(rattle)
# library(caret)
# library(leaps)
# library(Hmisc)
# library(randomForest)
# library(rpart)
# library(gbm)
# library(splines)
# library(parallel)
# library(plyr)
# 
# 
# #Read the data set
# data(diamonds)
# buildData <- diamonds
# str(buildData)
# 
# #Check the data for any near-zero variance variables
# nzVars <- nearZeroVar(buildData, saveMetrics = TRUE)
# nzVars
# 
# set.seed(1234)
# inTrain <- createDataPartition(y = buildData$price,
#                                p = 0.7,
#                                list = FALSE)
# training <- buildData[inTrain, ]
# testing <- buildData[-inTrain, ]
# trainCtrl <- trainControl(method='cv', number = 2)
# model <- c("price ~ carat")
# model <- as.formula(model)
# 
# set.seed(1234)
# fitLm <- train(model, data = training,
#                method = "lm",
#                trControl = trainCtrl)
# 
# 
# 
# predLm <- predict(fitLm, testing)
# 
# results <- rbind(
#         postResample(predLm, testing$price))
# 
# results <- as.data.frame(results)
# results$Model <- c("Linear Regression")
# results <- results[order(results$RMSE),]
# results
# 
# plot(x = testing$price, y = predLm)
# lines(predict(lm(buildData$price ~ buildData$carat)), type = "l")
# abline(fitLm)
# 
# 
# summary(fitLm)
# summary(fitRpart)
# summary(fitRandomForest)
# 
# 
# 
# 
# 
# 
# 
# set.seed(1234)
# fitRpart <- train(model,
#                   data = training,
#                   method = "rpart",
#                   tuneLength = 15,
#                   trControl = trainCtrl)
# 
# set.seed(1234)
# fitRandomForest <- train(model,
#                          data = training,
#                          method = "rf",
#                          tuneLength = 10,
#                          ntrees = 1000,
#                          importance = TRUE,
#                          trControl = trainCtrl)
# 
# predRpart <- predict(fitRpart, testing)
# predRandomForest <- predict(fitRandomForest, testing)
# 
# 
# data(wtloss)
# wind
# str(wtloss)
# ?wtloss
# min(range(diamonds$carat))
# max(range(diamonds$carat))
