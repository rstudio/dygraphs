library(dygraphs)

shinyUI(fluidPage(
  
  titlePanel("Shiny dycharts"),
  
  sidebarLayout(
    
    sidebarPanel(
      checkboxInput(inputId = "males", value = TRUE,
                    label = "Males Series"),
      checkboxInput(inputId = "females", value = TRUE,
                    label = "Females Series")
    ),
    
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))
