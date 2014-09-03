
library(dygraphs)
library(datasets)

# lung deaths
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
colnames(lungDeaths) <- c("All", "Male", "Female")

dygraph(lungDeaths, 
  series = list(
    dySeries("All", label = "Everyone"),
    dySeries("Male"),
    dySeries("Female", fillGraph = TRUE)
  ),
  axes = list(
    dyAxis("x", label = "Month"),
    dyAxis("y", label = "Deaths", drawGrid = FALSE)
  ),
  options = list(
    showRangeSelector = TRUE
  )
)

dygraph(discoveries, 
  title = "Important Discoveries",
  series = dySeries("Discoveries", strokeWidth = 2),
  axes = dyAxis("x", label = "Total / Year", pixelsPerLabel = 40),
  options = list(
    rollPeriod = 10,
    showRoller = TRUE
  )
)

