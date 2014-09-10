library(dygraphs)
library(datasets)

shinyServer(function(input, output) {
  
  lungDeaths <- reactive({
     if (input$males && input$females)
       cbind(ldeaths, mdeaths, fdeaths)
     else if (input$males)
       cbind(ldeaths, mdeaths)
     else if (input$females)
       cbind(ldeaths, fdeaths)
     else
       ldeaths
  })
  
  output$dygraph <- renderDygraph({

    dygraph(lungDeaths(), 
            main = "Monthly Deaths from Lung Diseases (UK)")
    
  })
  
})