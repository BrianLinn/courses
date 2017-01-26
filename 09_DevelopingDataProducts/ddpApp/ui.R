#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

library(shiny)
#shiny project requires two parts in the directory:
# One named ui.R (for user interface) controls how it looks.
# One named server.R that controls what it does.

#General Example
# shinyUI(fluidPage(
#         titlePanel("Data Science FTW!"),
#         sidebarLayout(
#                 sidebarPanel(
#                         h3("Sidebar Text")
#                 ),
#                 mainPanel(
#                         h3("Main Panel Text")
#                 )
#         )
# ))

#html tag examples
# shinyUI(pageWithSidebar(
#         headerPanel("Illustrating markup"),
#         sidebarPanel(
#                 h1('Sidebar panel'),
#                 h1('H1 text'),
#                 h2('H2 Text'),
#                 h3('H3 Text'),
#                 h4('H4 Text')
#                 
#         ),
#         mainPanel(
#                 h3('Main Panel text'),
#                 code('some code'),
#                 p('some ordinary text')
#         )
# ))

## Illustrating inputs ui.R
# shinyUI(pageWithSidebar(
#         headerPanel("Illustrating inputs"),
#         sidebarPanel(
#                 numericInput('id1', 'Numeric input, labeled id1', 0, min = 0, max = 10, step = 1),
#                 checkboxGroupInput("id2", "Checkbox",
#                                    c("Value 1" = "1",
#                                      "Value 2" = "2",
#                                      "Value 3" = "3")),
#                 dateInput("date", "Date:")  
#         ),
#         mainPanel(
#                 h3('Illustrating outputs'),
#                 h4('You entered'),
#                 verbatimTextOutput("oid1"),
#                 h4('You entered'),
#                 verbatimTextOutput("oid2"),
#                 h4('You entered'),
#                 verbatimTextOutput("odate")
#         )
# ))

# #Adding an input and output function
# shinyUI(fluidPage(
#         titlePanel("Slider App"),
#         sidebarLayout(
#                 sidebarPanel(
#                 h1("Move the Slider!"),
#                 sliderInput("slider2", "Slide Me!", 0, 100, 0)
#         ),
#         mainPanel(
#                 h3("Slider Value:"),
#                 textOutput("text1")
#         )
#         )
# ))

#Building the prediction function
library(shiny)
shinyUI(fluidPage(
        titlePanel("Plot Random Numbers"),
        sidebarLayout(
                sidebarPanel(
                        numericInput("numeric", "How Many Random Numbers Should be Plotted?",
                                     value = 1000, min = 1, max = 1000, step = 1),
                        sliderInput("sliderX", "Pick Minimum and Maximum X Values",
                                    -100, 100, value = c(-50, 50)),
                        sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
                                    -100, 100, value = c(-50, 50)),
                        checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
                        checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
                        checkboxInput("show_title", "Show/Hide Title")
                ),
                mainPanel(
                        h3("Graph of Random Points"),
                        plotOutput("plot1")
                )
        )
))