
library(dygraphs)

dygraph(discoveries, title = "Important Discoveries",
  yaxis = dyAxis(label = "Total / Year"),
  interaction = dyInteraction(showRangeSelector = TRUE)
)


