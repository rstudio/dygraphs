
library(dygraphs)

dygraph(discoveries, title = "Important Discoveries",
  yaxis = dyAxis(label = "Total / Year"),
  xaxis = dyAxis(label = "Discoveries"),
  interaction = dyInteraction(showRangeSelector = TRUE,
                              showLabelsOnHighlight = TRUE,
                              showRoller = FALSE)
)


