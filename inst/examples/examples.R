
library(datasets)
library(dygraphs)

discoveries %>%
  dygraph(title = "Important Discoveries", ylabel = "Total / Year") %>%
  rangeSelector(height = 45)
