
library(dygraphs)

dygraph(discoveries, title = "Important Discoveries",
  yaxis = dyAxis(label = "Total / Year"),
  xaxis = dyAxis(label = "Discoveries", legend = "Discoveries"),
  interaction = dyInteraction(legend = "onmouseover",
                              showRangeSelector = TRUE,
                              showLabelsOnHighlight = TRUE,
                              showRoller = FALSE),
  theme = dyTheme(titleHeight = 24,
                  xLabelHeight = 18,
                  yLabelWidth = 18)
)


