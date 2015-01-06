
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("RegularLogReg.R")

# datafile <- 'data/ex2data2.txt'
# dataset <- read.csv(datafile,header=F)
datafile <- 'data/gendata1.csv'
dataset <- read.csv(datafile)
values  <- as.matrix(dataset[,3])

extent = dim(values)[1]
train = sample(1:extent,size=0.8 * extent)

trainingdata <- dataset[train,1:2]
trainingvalues <- values[train]

validationdata <- dataset[-train,1:2]
validationvalues <- values[-train]

costfn <- function(p) {0.1*p[1]^2 + 0.1*p[2]^2 + 4*sin(p[1]) + 4*cos(p[2])}
costfn3 <- function(p) 0.1*(p[1]-3)^2 + 0.1*(p[2]+7)^2 + 3.1*sin(0.8*(p[1]+2)) + 2.4*cos(1.5*(p[2]-5))

xPts <- runif(1000, 0, 1)
yPts <- runif(1000, 0, 1)
zPts <- runif(1000, 0, 1)

library(shiny)
library(caret)
# library(shinyRGL)
# library(rgl)

shinyServer(function(input, output) {
  
  expandedTrainingData <- reactive( expandFeatures(as.matrix(trainingdata),input$degree))
  expandedValidationData <- reactive( expandFeatures(as.matrix(validationdata),input$degree))
  
  expandedData <- reactive({
    if(input$dataset == 'training') {
      expandedTrainingData()
    } else {
      expandedValidationData()
    }
  })

  initial_theta <- reactive( matrix(0,dim(expandedData())[2],1))
  
  confusionValues <- reactive({
    if(input$dataset == 'training') {
      trainingvalues
    } else {
      validationvalues
    }
  })
  
#  method <- "L-BFGS-B"
  method <- "BFGS"
  result <- reactive( optim(initial_theta(),
                         reglogregJ,
                         reglogregGrad,
                         method=method,
                         control=list(input$maxit),
                         as.matrix(expandedTrainingData()),
                         trainingvalues,
                         input$lambda))
  
  output$result <- reactive( result())
  output$convergence <- reactive( result()$convergence)
  
  confused <- reactive({
    pdata <- result()
    predictions <- expandedData() %*% pdata$par
    pred <- matrix(1,dim(predictions)[1],1)
    pred[ predictions < input$threshold] <- 0
    
    pred <- factor(pred,levels=c("1","0"))
    cv <- factor(confusionValues(),levels=c("1","0"))
    cm <- confusionMatrix(pred,cv,positive="1",dnn=c("Actual","Measured"))
  })
  
  output$confusion <- reactive( confused())
  
  output$cfxtable <- renderTable({ confused()$table })
  output$cfclass <- renderTable( as.table( confused()$byClass) )
  
#   output$sctPlot <- renderWebGL({
#     plotCosts(c(-20,20),c(-20,20),costfn3)
# #     points3d(xPts[1:input$pts],
# #              yPts[1:input$pts],
# #              zPts[1:input$pts])
# #     axes3d()
#   })

output$currentParams <- renderText({
  paste("Lambda:",
        input$lambda,
        ", Complexity: ",
        input$degree,
        ", Max Iterations: ",
        input$maxit,
        ", Threshold: ",
        input$threshold,
        ", Gridlines: ",
        input$gridlines)
})

output$plotParams <- renderText({
  input$replot
  isolate({
    paste("Lambda:",
          input$lambda,
          ", Complexity: ",
          input$degree,
          ", Max Iterations: ",
          input$maxit,
          ", Threshold: ",
          input$threshold,
          ", Gridlines: ",
          input$gridlines)
  })
})

output$distPlot <- renderPlot({
  input$replot
  isolate({
    plotdata <- result()
    plotDecisionContour(plotdata$par,expandedTrainingData(),trainingvalues,input$degree,input$gridlines,input$threshold)
  })
})

})
