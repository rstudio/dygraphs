library(dygraphs)

shinyUI(fluidPage(
  
  titlePanel("Predicted Deaths from Lung Disease (UK)"),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("months", label = "Months to Predict", 
                   value = 72, min = 12, max = 144, step = 12),
      selectInput("interval", label = "Prediction Interval",
                  choices = c("0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95")
    ),
    
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))
