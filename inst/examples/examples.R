
library(dygraphs)
library(datasets)

# lung deaths
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths) %>%
  dySeries("ldeaths", label = "All") %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyRangeSelector()


dygraph(discoveries, title = "Important Discoveries") %>%
  dyAxis("x", label = "Total / Year", pixelsPerLabel = 40) %>%
  dyAxis("y", label = "Discoveries") %>%
  dySeries(label = "Discoveries", fillGraph = TRUE) %>%
  dyRangeSelector() %>%
  dyOptions(drawGrid = TRUE, showRangeSelector = TRUE)

