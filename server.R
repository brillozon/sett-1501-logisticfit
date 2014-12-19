
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("RegularLogReg.R")

datafile <- 'data/ex2data2.txt'
dataset <- read.csv(datafile,header=F)
values  <- as.matrix(dataset[,3])

costfn <- function(p) {0.1*p[1]^2 + 0.1*p[2]^2 + 4*sin(p[1]) + 4*cos(p[2])}
costfn3 <- function(p) 0.1*(p[1]-3)^2 + 0.1*(p[2]+7)^2 + 3.1*sin(0.8*(p[1]+2)) + 2.4*cos(1.5*(p[2]-5))

xPts <- runif(1000, 0, 1)
yPts <- runif(1000, 0, 1)
zPts <- runif(1000, 0, 1)

library(shiny)
library(shinyRGL)
library(rgl)

shinyServer(function(input, output) {
  
  expandedData <- reactive( expandFeatures(as.matrix(dataset[,1:2]),input$degree))
  initial_theta <- reactive( matrix(0,dim(expandedData())[2],1))
  
  result <- reactive( optim(initial_theta(),
                         reglogregJ,
                         reglogregGrad,
                         method="L-BFGS-B",
                         control=list(input$maxit),
                         as.matrix(expandedData()),
                         values,
                         input$lambda))
  
  output$result <- reactive( result())
  
  output$distPlot <- renderPlot({
    plotdata <- result()
    plotDecisionContour(plotdata$par,expandedData(),values,input$degree,input$gridlines)
  })
  
  output$sctPlot <- renderWebGL({
    plotCosts(c(-20,20),c(-20,20),costfn3)
#     points3d(xPts[1:input$pts],
#              yPts[1:input$pts],
#              zPts[1:input$pts])
#     axes3d()
  })

})
