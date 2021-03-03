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
#'    \item{dyStackedBarChart()}{Draws a bar chart stacking all the underlying series.}
#'
#'    \item{dyMultiColumn()}{Draws multiple column bar chart.}
#
#'    \item{dyBarSeries()}{Draws a single set of bars for just the provided series.}
#'
#'    \item{dyStemSeries()}{Draws a single set of stems for just the provided series.}
#'
#'    \item{dyShadow()}{An extraction of the _fillplotter from dygraph-combined-dev.js,
#'    drawing the filled area without the line.}
#'
#'    \item{dyFilledLIne()}{An extraction of the _fillplotter and _lineplotter combo
#'    from dygraph-combined-dev.js. dyFilledLine allows users to fill only a single series.}
#'
#'    \item{dyMultiColumnGroup()}{The multicolumn plotter, but on a subset of the series, leaving
#'    the others for other plotters.}
#'
#'    \item{dyCandlestick()}{Draw a candlestick chart.}
#'
#'    \item{dyCandleStickGroup()}{Employed on the provided series,
#'    but still plotting the others.}
#'
#'    \item{dyStackerBarGroup()}{Return the data group as stacked bars}
#'
#'    \item{dyStackerRibbonGroup()}{Return the data group as stacked ribbons}
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
#' dygraph(mdeaths) %>%
#'   dyBarChart()
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyMultiColumn()
#'
#' # lungDeaths <- cbind(mdeaths, fdeaths)
#' # dygraph(lungDeaths) %>%
#' #   dyStackedBarChart()
#'
#' ## Per-series plotters:
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyBarSeries("fdeaths")
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyStemSeries("fdeaths")
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyShadow("fdeaths")
#'
#' lungDeaths <- cbind(mdeaths, fdeaths)
#' dygraph(lungDeaths) %>%
#'   dyFilledLine("fdeaths")
#'
#' ## A bunch of different plotters together:
#'
#' lungDeaths <- cbind(fdeaths, mdeaths, ldeaths, foo = fdeaths / 2, bar = fdeaths / 3)
#' dygraph(lungDeaths) %>%
#'   dyRangeSelector() %>%
#'   dyBarSeries("bar") %>%
#'   dyStemSeries("mdeaths") %>%
#'   dyShadow("foo") %>%
#'   dyFilledLine("fdeaths")
#'
#' ## Group-based plotters:
#'
#' # lungDeaths <- cbind(mdeaths, fdeaths, ldeaths)
#' # dygraph(lungDeaths) %>%
#' #   dyMultiColumnGroup(c('fdeaths', 'mdeaths'))
#'
#' ## Candlestick plotters:
#'
#' library(xts)
#' data(sample_matrix)
#' library(dygraphs)
#' dygraph(sample_matrix) %>%
#'   dyCandlestick()
#'
#' sample <- data.frame(sample_matrix)
#' sample_2 <- sample * 2
#' names(sample_2) <- c("O", "H", "L", "C")
#' sample <- cbind(sample, sample_2)
#' dygraph(sample) %>%
#'   dyOptions(stackedGraph = TRUE) %>%
#'   dyCandlestickGroup(c("Open", "High", "Low", "Close")) %>%
#'   dyCandlestickGroup(c("O", "H", "L", "C"))
#'
#' ## Stacked Bar and Ribbon Graphs:
#'
#' dygraph(lungDeaths) %>%
#'   dySeries("mdeaths", axis = "y2") %>%
#'   dyAxis("y", valueRange = c(-100, 1000)) %>%
#'   dyStackedBarGroup(c("ldeaths", "fdeaths"))
#'
#' lungDeaths <- cbind(ldeaths, fdeaths, mdeaths,
#'   additive = rep.int(200, length(ldeaths)),
#'   line = rep.int(3000, length(ldeaths))
#' )
#' dygraph(lungDeaths) %>%
#'   dySeries("line", strokePattern = "dashed") %>%
#'   dySeries("ldeaths", stepPlot = TRUE) %>%
#'   dyStackedBarGroup(c("additive", "mdeaths")) %>%
#'   dyStackedRibbonGroup(c("fdeaths", "ldeaths"))
NULL

#' @rdname Plotters
#' @export
dyBarChart <- function(dygraph) {
  if (length(dygraph$x$data) > 2) {
    dygraph <- dyPlotter(
      dygraph = dygraph,
      name = "MultiColumn",
      path = system.file("plotters/multicolumn.js", package = "dygraphs")
    )
  } else {
    dygraph <- dyPlotter(
      dygraph = dygraph,
      name = "BarChart",
      path = system.file("plotters/barchart.js", package = "dygraphs")
    )
  }

  return(dygraph)
}


#' @rdname Plotters
#' @export
dyStackedBarChart <- function(dygraph) {
  dygraph <- dyPlotter(
    dygraph = dygraph,
    name = "StackedBarChart",
    path = system.file("plotters/stackedbarchart.js", package = "dygraphs")
  )

  names <- dygraph$x$attrs$labels[-1]

  dygraph <- computeYaxisRange(dygraph, names)
  dygraph
}


#' @rdname Plotters
#' @export
dyMultiColumn <- function(dygraph) {
  dyPlotter(
    dygraph = dygraph,
    name = "MultiColumn",
    path = system.file("plotters/multicolumn.js", package = "dygraphs")
  )
}


#' @rdname Plotters
#' @export
dyBarSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/barseries.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dySeries", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyStemSeries <- function(dygraph, name, ...) {
  file <- system.file("plotters/stemplot.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dySeries", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyShadow <- function(dygraph, name, ...) {
  file <- system.file("plotters/fillplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dySeries", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyFilledLine <- function(dygraph, name, ...) {
  file <- system.file("plotters/filledline.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dySeries", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyErrorFill <- function(dygraph, name, ...) {
  file <- system.file("plotters/errorplotter.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dySeries", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyMultiColumnGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/multicolumngroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dyGroup", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyCandlestickGroup <- function(dygraph, name, ...) {
  file <- system.file("plotters/candlestickgroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dots <- list(...)
  do.call("dyGroup", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))
}


#' @rdname Plotters
#' @export
dyStackedBarGroup <- function(dygraph, name, ...) {
  dots <- list(...)

  if (length(name) < 2) {
    dygraph <- do.call("dyBarSeries", c(list(dygraph = dygraph, name = unlist(name)), dots))
    return(dygraph)
  }

  file <- system.file("plotters/stackedbargroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dygraph <- do.call("dyGroup", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))

  dygraph <- computeYaxisRange(dygraph, name)
  dygraph
}

#' @rdname Plotters
#' @export
dyStackedLineGroup <- function(dygraph, name, ...) {
  dots <- list(...)

  if (length(name) < 2) {
    # just rely on the base package
    return(dygraph)
  }

  file <- system.file("plotters/stackedlinegroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dygraph <- do.call("dyGroup", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))

  dygraph <- computeYaxisRange(dygraph, name)
  dygraph
}


#' @rdname Plotters
#' @export
dyStackedRibbonGroup <- function(dygraph, name, ...) {
  dots <- list(...)

  if (length(name) < 2) {
    dygraph <- do.call("dyFilledLine", c(list(dygraph = dygraph, name = name), dots))
    return(dygraph)
  }

  file <- system.file("plotters/stackedribbongroup.js", package = "dygraphs")
  plotter_ <- paste0(readLines(file, skipNul = T), collapse = "\n")

  dygraph <- do.call("dyGroup", c(list(dygraph = dygraph, name = name, plotter = plotter_), dots))

  dygraph <- computeYaxisRange(dygraph, name)
  dygraph
}

computeYaxisRange <- function(dygraph, name) {
  # most of what happens from here on out is a simplified version of the
  # stackPoints and computeYaxis functions in the underlying dygraph package.
  # Since we can't modify the Yaxis range from within the specialized plotter,
  # we need to calculate an appropriate valueRange for the group's axis here, then
  # reconcile that against user-provided ranges and then pass into the widget... this
  # way we ensure that the newly stacked data (but initially unstacked to the widget)
  # doesn't get cutoff by an axis range - computed by the widget - that won't consider
  # the points stacked
  attrs <- dygraph$x$attrs

  data <- attr(dygraph$x, "data")
  cols <- which(names(data) %in% name)

  # get all the series, minus the x axis, that are not part of the group
  data_ <- data[-c(1, cols)]
  name_ <- names(data)[which(!names(data) %in% name)][-1]

  # grab the axis for the group, we'll calculate the new range only on this axis
  series <- attrs$series[[name[1]]]
  axisNm <- series$axis
  if (!is.null(axisNm)) {
    axis <- attrs$axes[[axisNm]]
    valueRange <- axis$valueRange
  } else {
    axisNm <- "y"
    axis <- attrs$axes[[axisNm]] <- NULL
    valueRange <- NULL
  }

  if (is.null(valueRange)) {
    valueRange <- c(0, 0)
  }

  # get the group data fields
  data <- data[cols]

  for (i in 1:length(data)) {
    # get the data points
    points <- data[i][[1]]
    # fill NAs... we're not saving this data back into the graph, so this is OK
    is.na(points) <- 0

    # add to cumulativeList
    if (!exists("cumulativeYval")) {
      cumulativeYval <- points
    } else {
      cumulativeYval <- cumulativeYval + points
    }
    # calculate extremes
    extremes <- range(cumulativeYval)
  }

  for (i in 1:length(data_)) {
    if (length(data_) == 0) break

    # ranges are calcuated separately, so skip those from other axes
    series_ <- attrs$series[[name_[i]]]
    if (!is.null(series_$axis) && series_$axis != axisNm) next

    points <- data_[i][[1]]

    # fill NAs
    is.na(points) <- 0

    # getExtremes
    extremes_ <- range(points)
    extremes[1] <- min(extremes[1], extremes_[1])
    extremes[2] <- max(extremes[2], extremes_[2])
  }

  valueRange[1] <- min(extremes[1], valueRange[1])
  valueRange[2] <- max(extremes[2], valueRange[2])

  # add a little padding since we're hard-setting the range
  valueRange[2] <- valueRange[2] + 0.05 * abs(valueRange[2] - valueRange[1])

  axis$options$valueRange <- valueRange
  attrs$axes[[axisNm]] <- axis$options

  # return modified dygraph
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, attrs)
  return(dygraph)
}
