
# Demonstrate the effects of over and under fitting a model to data.

library(shiny)
library(shinyRGL)

shinyUI(fluidPage(

  # Application title
  titlePanel("Fitting Data to a Model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  div(p("Lambda"),p("(Regularization Parameter)")),
                  min = 0.0, max = 10.0,
                  value = 1.0, step=0.1),
      hr(),
      wellPanel(
        numericInput("degree", label = div(h4('Complexity'),p('[1-17]')),
                     value = 6, min = 1, max = 17, step = 1),
        numericInput("maxit", label = div(h4('Maximum Iterations'),p('[1-700]')),
                     value = 400, min = 1, max = 700, step = 1),
        numericInput("gridlines", label = div(h4('Contour Gridlines'),p('[50-200]')),
                     value = 100, min = 50, max = 200, step = 1),
        textOutput("result$convergence")
      )  # ,
#       sliderInput("pts", 
#                   "Number of points in 3d scatterplot:", 
#                   min = 10, 
#                   max = 1000, 
#                   value = 250)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot",width="600px",height="600px"),
      webGLOutput("sctPlot")
    )
  )
))
