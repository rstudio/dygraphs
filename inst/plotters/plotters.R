# The dyBarChart plotter draws a bar plot rather than a line plot.
dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js", package = "dygraphs"))
}

# The dyMultiColumn plotter draws multiple column bar chart.
dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = system.file("plotters/multicolumn.js", package = "dygraphs"))
}

# Our plotter wrapper functions can now be incorporated directly into a dygraph
library(dygraphs)
dygraph(ldeaths) %>%
  dyRangeSelector() %>%
  dyBarChart()

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths) %>%
  dyRangeSelector() %>%
  dyMultiColumn()
