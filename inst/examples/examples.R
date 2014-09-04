
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


temperature <- ts(
  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
  frequency = 12, start = c(1980, 1))
rainfall <- ts(
  data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4),
  frequency = 12, start = c(1980, 1))
weather <- cbind(rainfall, temperature)

dygraph(weather,
  series = dySeries('rainfall', axis = 'y2'),
)




