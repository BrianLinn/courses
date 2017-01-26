library(shiny)
# shinyServer(
#         function(input, output) {
#                 output$text1 = renderText(input$slider2)
#         }
# )
# 
# shinyServer(
#         function(input, output) {
#                 output$oid1 <- renderPrint({input$id1})
#                 output$oid2 <- renderPrint({input$id2})
#                 output$odate <- renderPrint({input$date})
#         }
# )

#Apps with unputs
shinyServer(function(input, output) {
        output$plot1 <- renderPlot({
                set.seed(2016-05-25)
                number_of_points <- input$numeric
                minX <- input$sliderX[1]
                maxX <- input$sliderX[2]
                minY <- input$sliderY[1]
                maxY <- input$sliderY[2]
                dataX <- runif(number_of_points, minX, maxX)
                dataY <- runif(number_of_points, minY, maxY)
                xlab <- ifelse(input$show_xlab, "X Axis", "")
                ylab <- ifelse(input$show_ylab, "Y Axis", "")
                main <- ifelse(input$show_title, "Title", "")
                plot(dataX, dataY, xlab = xlab, ylab = ylab, main = main,
                     xlim = c(-100, 100), ylim = c(-100, 100))
        })
})