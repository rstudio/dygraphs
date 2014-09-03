
library(dygraphs)
library(datasets)

# lung deaths
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths)


dygraph(discoveries, title = "Important Discoveries") %>%
  dyAxis("x", label = "Total / Year") %>%
  dyAxis("y", label = "Discoveries") %>%
  dyRangeSelector()

