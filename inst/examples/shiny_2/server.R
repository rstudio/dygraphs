shinyServer(function(input, output) {
  # map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      setView(lng=70, lat=-20, zoom=2)
  })  
  # dygraph
  output$dygraph <- renderDygraph({
    dygraph(bird.monthly, main='Fork-tailed Swift') %>%
      dySeries('V1', drawPoints=TRUE, label='GBIF Observations') %>%
      dyRangeSelector() %>%
      dySliderInput(color='red', strokePattern='dashed', animate=animationOptions(interval=2000, loop=TRUE))
  })  
  # update map based on dygraph
  observe({
    if (length(input$dygraph_click$x_closest_point) > 0) {
      # clear map
      p <- leafletProxy('map') %>%
        clearMarkers() %>%
        clearMarkerClusters()
    
      # update map with new data
      if (!is.na(input$dygraph_click$x_closest_point)) {
        # get date
        curr.ym <- format(strptime(input$dygraph_click$x_closest_point, '%b %d, %Y %H:%M:%S'), format='%Y-%m')
        # generate new data to plot
        if (!is.na(curr.ym)) {
          curr.obs <- bird %>% filter(year_month == curr.ym)
          # add new data to plot
          p <- p %>% addMarkers(lng=curr.obs$longitude, lat=curr.obs$latitude,
            clusterOptions=markerClusterOptions(), popup=paste0(curr.obs$label))
        }
      }
      
      # render map
      p
    }
  })
})
