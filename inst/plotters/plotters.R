# The dyBarChart plotter draws a bar plot rather than a line plot.
dyBarChart <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "BarChart",
            path = system.file("plotters/barchart.js", package = "dygraphs"))
}

# Our plotter wrapper functions can now be incorporated directly into a dygraph
library(dygraphs)
dygraph(ldeaths) %>%
  dyRangeSelector() %>%
  dyBarChart()

# The dyMultiColumn plotter draws multiple column bar chart.
dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = system.file("plotters/multicolumn.js", package = "dygraphs"))
}

lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths) %>%
  dyRangeSelector() %>%
  dyMultiColumn()

#the dyBarSeries draws a single set of bars to just the provided series
dyBarSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/barseries.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_,
           ...)
}

#the dyStemPlot draws a single set of bars to just the provided series
dyStemSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/stemplot.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#extraction of the _lineplotter from dygraph-combined-dev.js
dyLineSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/lineplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#extraction of the _fillplotter from dygraph-combined-dev.js
dyShadow <- function(dygraph, name, ...) {
  file <- system.file("plotters/fillplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#extraction of the _fillplotter, _lineplotter combo 
# from dygraph-combined-dev.js
dyFilledLine <- function(dygraph, name, ...) {
  file <- system.file("plotters/filledline.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#extraction of the _errorplotter from dygraph-combined-dev.js
dyErrorFill <- function(dygraph, name, ...) {
  file <- system.file("plotters/errorplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#extraction of the _errorplotter, _lineplotter combo 
# from dygraph-combined-dev.js
dyErrorSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/errorline.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

# bunch of different plotters together
lungDeaths <- cbind(fdeaths, mdeaths, ldeaths, test = fdeaths/2, test2 = fdeaths/3)
dygraph(lungDeaths) %>%
  dyRangeSelector() %>%
  dyErrorSeries(c('fdeaths', 'mdeaths', 'ldeaths')) %>% 
  dyShadow('test') %>% 
  dyFilledLine('test2')

# the multicolumn plotter, but only on a subset of the series, leaving the others for other plotters
dyMultiColumnGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/multicolumngroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dyGroup(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
  
}

lungDeaths <- cbind(mdeaths, fdeaths, ldeaths)
dygraph(lungDeaths) %>% 
  dyMultiColumnGroup(c('fdeaths', 'mdeaths'))

# the candlestick plotter, both the original and the dyGroup version
library(xts)
data(sample_matrix)
library(dygraphs)
dygraph(sample_matrix) %>%
  dyCandlestick()

dyCandlestickGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/candlestickgroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dyGroup(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

sample<-data.frame(sample_matrix)
sample_2<-sample*2
names(sample_2)<-c('O', 'H', 'L', 'C')
sample<-cbind(sample, sample_2)
dygraph(sample) %>% 
  dyOptions(stackedGraph = TRUE) %>% 
  dyCandlestickGroup(c('Open', 'High', 'Low', 'Close')) %>% 
  dyCandlestickGroup(c('O', 'H', 'L', 'C'))
  