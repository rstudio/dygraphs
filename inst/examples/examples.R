
library(dygraphs)

dygraph(discoveries, title = "Important Discoveries",
  yaxis = dyAxis(label = "Total / Year"),
  xaxis = dyAxis(legend = "Discoveries"),
  interaction = dyInteraction(legend = "onmouseover",
                              showRangeSelector = TRUE,
                              showLabelsOnHighlight = TRUE,
                              showRoller = FALSE)
)


