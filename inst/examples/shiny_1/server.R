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
  
  output$from <- renderText({
    format(strptime(req(input$dygraph_date_window[[1]]), '%b %d, %Y %H:%M:%S'), '%Y/%m/%d %H:%M:%S') 
  })
  
  output$to <- renderText({
    format(strptime(req(input$dygraph_date_window[[2]]), '%b %d, %Y %H:%M:%S'), '%Y/%m/%d %H:%M:%S')
  })
  
  output$clicked <- renderText({
    format(strptime(req(input$dygraph_click$x), '%b %d, %Y %H:%M:%S'), '%Y/%m/%d %H:%M:%S')
  })

  output$point <- renderText({
    paste0(
      'X = ', format(strptime(req(input$dygraph_click$x_closest_point), '%b %d, %Y %H:%M:%S'), '%Y/%m/%d %H:%M:%S'),
      '; Y = ', req(input$dygraph_click$y_closest_point)
     )
  })
  
})
