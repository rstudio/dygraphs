#' Employ a dygraph plotter on a series, a group of series, or the whole dygraph
#' 
#' Plotters provide variuos ways to customize how your data appears
#' on the dygraph.  Series-based plotters allow users to mix-and-match different plotters on a 
#' per-series or (with dyGroup) a per-group basis.  See \code{\link{dyPlotter}} for additional detail.
#'
#'
#' @section Available plotters:
#'
#' 
#' Currently the dygraphs package provides the following plotters:
#' 
#' \describe{
#'    \item{dyBarChart()}{Draws a bar plot rather than a line plot. If the provided dygraph
#' features more than one series, dyBarChart will call dyMultiColumn instead.}
#'
#'    \item{dyMultiColumn()}{Draws multiple column bar chart.} 
# 
#'    \item{dyBarSeries()}{Draws a single set of bars for just the provided series.}
#' 
#'    \item{dyStemSeries()}{Draws a single set of bars to just the provided series.}
#' 
#'    \item{dyShadow()}{An extraction of the _fillplotter from dygraph-combined-dev.js, 
#'    drawing the filled area without the line.}
#' 
#'    \item{dyFilledLIne()}{An extraction of the _fillplotter and _lineplotter combo 
#'    from dygraph-combined-dev.js. dyFilledLine allows users to fill only a single series.}
#' 
#'    \item{dyMultiColumnGroup()}{The multicolumn plotter, but on a subset of the series, leaving 
#'    the others for other plotters. Utilizes [dyGroup()].}
#' 
#'    \item{dyCandlestick()}{Draw a candlestick chart.}
#' 
#'    \item{dyCandleStickGroup()}{Employed on the provided series, 
#'    but still plotting the others.}
#' 
#' }
#' 
#' 
#' @param dygraph Dygraph to add plotter to
#' @param name name - or chrarcter vector of names - of (the) series within the data set
#' @param compress (For dyCandlestick) If true, compress data yearly, quarterly, monthly, weekly or daily
#' according to overall amount of bars and/or current zoom level.
#' @param ... additional options to pass to dySeries
#'
#'
#' @return A dygraph with the specified plotter(s) employed.
#' 
#' @name Plotters
#' @examples 
#' ## The following two examples will results in the same dygraph:
#'  
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyMultiColumn()
#'   
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyBarChart()
#'   
#' ## Per-series plotters:
#' 
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyBarSeries('fdeaths')
#' 
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyStemSeries('fdeaths')
#'   
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyShadow('fdeaths')
#' 
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyFilledLine('fdeaths')
#'   
#' ## A bunch of different plotters together
#' 
#' lungDeaths <- cbind(fdeaths, mdeaths, ldeaths, foo = fdeaths/2, bar = fdeaths/3)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyBarSeries('bar') %>% 
#'   dyStemSeries('mdeaths') %>% 
#'   dyShadow('foo') %>% 
#'   dyFilledLine('fdeaths')
#'   
#' ## Group-based plotters:
#'   
#' lungDeaths <- cbind(mdeaths, fdeaths, ldeaths)
#' dygraph(lungDeaths) %>% 
#'   dyMultiColumnGroup(c('fdeaths', 'mdeaths'))
#'
#' ## Candlestick plotters:
#'   
#' library(xts)
#' data(sample_matrix)
#' library(dygraphs)
#' dygraph(sample_matrix) %>%
#'   dyCandlestick()
#'   
#' sample<-data.frame(sample_matrix)
#' sample_2<-sample*2
#' names(sample_2)<-c('O', 'H', 'L', 'C')
#' sample<-cbind(sample, sample_2)
#' dygraph(sample) %>% 
#'   dyOptions(stackedGraph = TRUE) %>% 
#'   dyCandlestickGroup(c('Open', 'High', 'Low', 'Close')) %>% 
#'   dyCandlestickGroup(c('O', 'H', 'L', 'C'))
NULL

#' @rdname Plotters
#' @export
dyBarChart <- function(dygraph) {
  if (length(dygraph$x$data)>2) {
    dygraph <- dyPlotter(dygraph = dygraph,
              name = "MultiColumn",
              path = system.file("plotters/multicolumn.js", package = "dygraphs"))
    
  } else {
    dygraph <- dyPlotter(dygraph = dygraph,
              name = "BarChart",
              path = system.file("plotters/barchart.js", package = "dygraphs"))
  }
  
  return (dygraph)
}


#' @rdname Plotters
#' @export
dyMultiColumn <- function(dygraph) {
  dyPlotter(dygraph = dygraph,
            name = "MultiColumn",
            path = system.file("plotters/multicolumn.js", package = "dygraphs"))
}

#' @rdname Plotters
#' @export
dyBarSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/barseries.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_,
           ...)
}

#' @rdname Plotters
#' @export
dyStemSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/stemplot.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#' @rdname Plotters
#' @export
dyShadow <- function(dygraph, name, ...) {
  file <- system.file("plotters/fillplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#' @rdname Plotters
#' @export
dyFilledLine <- function(dygraph, name, ...) {
  file <- system.file("plotters/filledline.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}

#' @rdname Plotters
#' @export
dyErrorFill <- function(dygraph, name, ...) {
  file <- system.file("plotters/errorplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dySeries(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}


#' @rdname Plotters
#' @export
dyMultiColumnGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/multicolumngroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dyGroup(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
  
}

#' @rdname Plotters
#' @export
dyCandlestickGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/candlestickgroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dyGroup(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
}


#' @rdname Plotters
#' @export
dyStackedBarGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/stackedbargroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")
  dyGroup(dygraph = dygraph, 
           name = name, 
           plotter = plotter_, 
           ...)
  
}
