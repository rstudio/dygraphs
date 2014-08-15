
library(dygraphs)

discoveries %>%
  dygraph(title = "Important Discoveries", ylabel = "Total / Year") %>%
  rangeSelector()
