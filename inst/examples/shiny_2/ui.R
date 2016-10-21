shinyUI(fluidPage(
  div(class='outer', 
    tags$head(
      includeCSS('styles.css')
    ),
    leafletOutput('map', width='100%', height='100%'),
    absolutePanel(
      id='controls', class='panel panel-default', fixed=TRUE, draggable=FALSE, top='auto', left='5%', right='5%', bottom=1, width='auto', height='auto',
      dygraphOutput('dygraph', height=175)
    )
  )
))
