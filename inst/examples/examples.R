
library(dygraphs)

dygraph(discoveries, title = "Important Discoveries") %>%
  dyRangeSelector() %>%
  dyRoll(5, showRoller = TRUE) %>%
  dyAxis("y", label = "Total / Year")

