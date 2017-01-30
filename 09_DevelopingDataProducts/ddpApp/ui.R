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

###############################################################################
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

###############################################################################
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

###############################################################################
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

###############################################################################
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

###############################################################################
# #Apps with inputs
# library(shiny)
# shinyUI(fluidPage(
#         titlePanel("Plot Random Numbers"),
#         sidebarLayout(
#                 sidebarPanel(
#                         numericInput("numeric", "How Many Random Numbers Should be Plotted?",
#                                      value = 1000, min = 1, max = 1000, step = 1),
#                         sliderInput("sliderX", "Pick Minimum and Maximum X Values",
#                                     -100, 100, value = c(-50, 50)),
#                         sliderInput("sliderY", "Pick Minimum and Maximum Y Values",
#                                     -100, 100, value = c(-50, 50)),
#                         checkboxInput("show_xlab", "Show/Hide X Axis Label", value = TRUE),
#                         checkboxInput("show_ylab", "Show/Hide Y Axis Label", value = TRUE),
#                         checkboxInput("show_title", "Show/Hide Title")
#                 ),
#                 mainPanel(
#                         h3("Graph of Random Points"),
#                         plotOutput("plot1")
#                 )
#         )
# ))

###############################################################################
# #Apps with reactive expressions
# library(shiny)
# shinyUI(fluidPage(
#         titlePanel("Predict Horespower from MPG"),
#         sidebarLayout(
#                 sidebarPanel(
#                         sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
#                         checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
#                         checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
#                         submitButton("Submit")
#                 ),
#                 mainPanel(
#                         plotOutput("plot1"),
#                         h3("Predicted Horsepower from Model 1:"),
#                         textOutput("pred1"),
#                         h3("Predicted Horespower from Model 2:"),
#                         textOutput("pred2")
#                 )
#         )
# ))

###############################################################################
# #Tabs
# library(shiny)
# shinyUI(fluidPage(
#         titlePanel("Tabs!"),
#         sidebarLayout(
#                 sidebarPanel(
#                         textInput("box1", "Enter Tab 1 Text:", value = "Tab 1!"),
#                         textInput("box2", "Enter Tab 2 Text:", value = "Tab 2!"),
#                         textInput("box3", "Enter Tab 3 Text:", value = "Tab 3!")
#                 ),
#                 mainPanel(
#                         tabsetPanel(type = "tabs",
#                                     tabPanel("Tab 1", br(), textOutput("out1")),
#                                     tabPanel("Tab 2", br(), textOutput("out2")),
#                                     tabPanel("Tab 3", br(), textOutput("out3"))
#                                         )
#                                 )
#                 )
#         )
# )

###############################################################################
#Interactive Graphics
# library(shiny)
# shinyUI(fluidPage(
#         titlePanel("Visualize Many Models"),
#         sidebarLayout(
#                 sidebarPanel(
#                         h3("Slope"),
#                         textOutput("slopeOut"),
#                         h3("Intercept"),
#                         textOutput("intOut")
#                 ),
#                 mainPanel(
#                         plotOutput("plot1", brush = brushOpts(
#                                 id = "brush1"
#                         ))
#                 )
#         )
# ))

###############################################################################
#Quiz
shinyUI(pageWithSidebar(  
        headerPanel("Example plot"),  
        sidebarPanel(    
                sliderInput('mu', 'Guess at the mu',value = 70, min = 60, max = 80, step = 0.05,)  ), 
        mainPanel(    
                plotOutput('newHist')  
        )
))