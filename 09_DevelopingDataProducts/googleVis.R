#Google Vis
# The R function creates an HTML page
# I The HTML page calls Google Charts
# I The result is an interactive HTML graphic

################################################################################
#Charts in googleVis
suppressPackageStartupMessages(library(googleVis))
M <- gvisMotionChart(Fruits, "Fruit", "Year",
                     options = list(width = 600, height = 400))
print(M, "chart")

# “gvis + ChartType”
# I Motion charts: gvisMotionChart
# I Interactive maps: gvisGeoChart
# I Interactive tables: gvisTable
# I Line charts: gvisLineChart
# I Bar charts: gvisColumnChart
# I Tree maps: gvisTreeMap
# http://cran.r-project.org/web/packages/googleVis/googleVis.pdf

################################################################################
#Plots on Maps
G <- gvisGeoChart(Exports, locationvar = "Country",
                  colorvar = "Profit", options = list(width = 600, height = 400))
print(G, "chart")
#Specifying a Region on the map
G2 <- gvisGeoChart(Exports, locationvar = "Country",
                  colorvar = "Profit", options = list(width = 600, height = 400,
                                                      region = "150"))
print(G2, "chart")


################################################################################
#Charts and more options
df <- data.frame(label = c("US", "GB",  "BR"), val1 = c(1, 3, 4), val2 = c(23, 12, 32))
Line <- gvisLineChart(df, xvar="label", yvar=c("val1","val2"),
                      options=list(title="Hello World", legend="bottom",
                                   titleTextStyle="{color:'red', fontSize:18}",                         
                                   vAxis="{gridlines:{color:'red', count:3}}",
                                   hAxis="{title:'My Label', titleTextStyle:{color:'blue'}}",
                                   series="[{color:'green', targetAxisIndex: 0}, 
                                                {color: 'blue',targetAxisIndex:1}]",
                                   vAxes="[{title:'Value 1 (%)', format:'##,######%'}, 
                                                {title:'Value 2 (\U00A3)'}]",                          
                                   curveType="function", width=500, height=300                         
                      ))

print(Line, "chart")


################################################################################
#Combining multiple plots together
G <- gvisGeoChart(Exports, "Country", "Profit",options=list(width=200, height=100))
T1 <- gvisTable(Exports,options=list(width=200, height=270))
M <- gvisMotionChart(Fruits, "Fruit", "Year", options=list(width=400, height=370))
GT <- gvisMerge(G,T1, horizontal=FALSE)
GTM <- gvisMerge(GT, M, horizontal=TRUE,tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10")

print(GTM,"chart")

