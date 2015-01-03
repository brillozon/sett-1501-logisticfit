
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
                  div(h4("Lambda:"),p("(Regularization Parameter)")),
                  min = 0.0, max = 10.0,
                  value = 1.0, step=0.1),
      hr(),
      wellPanel(
        sliderInput("degree", label = div(h6('Complexity:'),p('[1-17]')),
                     value = 6, min = 1, max = 17, step = 1),
        sliderInput("maxit", label = div(h6('Maximum Iterations:'),p('[1-700]')),
                     value = 400, min = 1, max = 700, step = 1),
        sliderInput("gridlines", label = div(h6('Contour Gridlines:'),p('[50-200]')),
                     value = 100, min = 50, max = 200, step = 1),
        sliderInput("threshold", label = div(h6("Threshold:"),p('[0.0-1.0]')),
                     value = 0.0, min = 0.0, max = 1.0, step = 0.01)
      ),
      span(HTML("Convergence:"),
               textOutput("convergence",inline="true")),
      p("(0 - converged"), p("1 - max iterations)")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      textOutput("currentParams"),
      fluidRow(
        column(4,
               selectInput('dataset',"Confusion Matrix Dataset",
                           c("Training" = 'training',"Validation" = 'validation'),
                           selected='validation'),
               htmlOutput("cfxtable")
        ),
        column(8,
          htmlOutput("cfclass")
        )
      ),
      hr(),
      textOutput("plotParams"),
      actionButton("replot",label="Replot Training Data"),
      plotOutput("distPlot",width="600px",height="600px")
      # , hr(), webGLOutput("sctPlot")
    )
  )
))
