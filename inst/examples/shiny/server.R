library(dygraphs)
library(datasets)

shinyServer(function(input, output) {
  
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
})
