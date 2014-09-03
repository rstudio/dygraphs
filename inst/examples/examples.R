
library(dygraphs)
library(datasets)

# lung deaths
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
colnames(lungDeaths) <- c("All", "Male", "Female")
dygraph(lungDeaths) %>%
  dyRangeSelector() %>%
  dyRoll(10, showRoller = TRUE)
 


dygraph(discoveries, title = "Important Discoveries") %>%
  dyAxis("x", label = "Total / Year", pixelsPerLabel = 40) %>%
  dyAxis("y", label = "Discoveries") %>%
  dySeries(label = "Discoveries", fillGraph = TRUE) %>%
  dyRangeSelector() %>%
  dyOptions(drawGrid = TRUE)

