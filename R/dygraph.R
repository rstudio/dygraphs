
#' Interactive plot for time series data
#' 
#' R interface to interactive time series plotting using the 
#' \href{http://dygraphs.com}{dygraphs} JavaScript library.
#' 
#' @param data Time series data (must be an \link[xts]{xts} object or 
#'   object which is covertible to \code{xts}).
#' @param title Main plot title (optional)
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#'   
#' @return Interactive dygraph plot
#' 
#' @export
dygraph <- function(data, title = NULL, width = NULL, height = NULL) {
  
  # convert to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity 
  periodicity <- periodicity(data)
  
  # convert time string we can pass to javascript Date function and
  # extract core data from xts object
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  data <- coredata(data)
  
  # calculate column names
  colNames <- colnames(data)
  if (is.null(colNames))
    colNames <- paste("V", 1:ncol(data), sep="")
  
  # merge time into data then strip the metadata (so that the data is 
  # marshalled as a 2d array into json)
  data <- cbind(time, as.data.frame(data), stringsAsFactors = FALSE)
  data <- unclass(data)
  names(data) <- NULL
  
  # convert to native dygraph json options format
  x <- list()
  x$file <- data
  x$title <- title
  x$labels <- c(periodicity$label, colNames)
  if (length(colNames) > 1)
    x$legend <- "always"
  x$axes$x <- list()
  
  # side data we use in javascript
  meta <- list()
  meta$scale <- periodicity$scale
  x$meta <- meta
  
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10, browser.fill = TRUE)
  )
}

#' @importFrom magrittr %>%
#' @export %>%
NULL


#' dygraph axis options
#' 
#' Add per-axis options to a dygraph plot.
#' 
#' @param dygraph Plot to add options to
#' @param name Name of axis ('x', 'y', or 'y2')
#' @param label Label to display for axis (defaults to none)
#' @param drawGrid Whether to display gridlines in the chart for this axis (uses
#'   the global default if not specified).
#' @param ... Additional per-axis options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Interactive dygraph plot
#'   
#' @export
dyAxis <- function(dygraph, 
                   name, 
                   label = NULL, 
                   drawGrid = NULL,
                   ...) {
  
  # validate name
  if (!name %in% c("x", "y", "y2"))
    stop("Invalid axis name (must be 'x', 'y', or 'y2')")
  
  # axis options
  options <- list()
  options[[sprintf("%slabel", name)]] <- label
  options$axes[[name]]$drawGrid <- drawGrid
  
  # add var args
  options$axes[[name]] <- mergeLists(options$axes[[name]], list(...))
  
  # merge with main options
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}

#' dygraph data series options
#' 
#' Add per-series options to a dygraph plot.
#' 
#' @param dygraph Plot to add options to
#' @param name Name of series (this can be excluded if there is only one series)
#' @param label Label to display for series (default to name)
#' @param fillGraph Should the area underneath the graph be filled? (uses the
#'   global default if not specified).
#' @param ... Additional per-series options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Interactive dygraph plot
#'   
#' @export
dySeries <- function(dygraph, 
                     name = NULL, 
                     label = NULL, 
                     fillGraph = NULL,
                     ...) {
  
  # we can deduce the name only if there is one series
  if (is.null(name)) {
    if (length(dygraph$x$labels) == 2)
      name <- dygraph$x$labels[[2]]
    else
      stop("More than one series so must specify a series name")
  } 
  
  # replace default label (this also becomes the name)
  if (!is.null(label)) {
    dygraph$x$labels[dygraph$x$labels == name] <- label
    name <- label
  }
  
  # per-series options
  options <- list()
  options$fillGraph <- fillGraph
  
  # add varargs
  options <- mergeLists(options, list(...))
  
  # set and return
  dygraph$x$series[[name]] <- options
  dygraph
}

#' @export
dyLegend <- function(dygraph, always = FALSE, hideOnMouseOut = TRUE) {
  
  # legend options
  options <- list()
  options$legend <- ifelse(always, "always", "onmouseover")
  options$hideOverlayOnMouseOut <- hideOnMouseOut
  
  # merge with main options
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}

#' @export
dyRangeSelector <- function(dygraph,
                            height = 40,  
                            plotFillColor = "#A7B1C4", 
                            plotStrokeColor = "#A7B1C4") {
  options <- list()
  options$showRangeSelector <- TRUE
  options$rangeSelectorHeight <- height
  options$rangeSelectorPlotFillColor <- plotFillColor
  options$rangeSelectorPlotStrokeColor <- plotStrokeColor
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}

#' @export
dyRoll <- function(dygraph, rollPeriod = 1, showRoller = FALSE) {
  options <- list()
  options$rollPeriod = rollPeriod
  options$showRoller = showRoller
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}


#' dygraph options
#' 
#' Add options to a dygraph plot
#' 
#' @param drawGrid Whether to display gridlines in the chart. This may also be
#'   set on a \link[=dyAxis]{per-axis basis}.
#' @param fillGraph Should the area underneath the graph be filled? This may
#'   also be set on a \link[=dySeries]{per-series basis}
#' @param ... Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Interactive dygraph plot
#'   
#' @export
dyOptions <- function(dygraph, 
                      drawGrid = TRUE, 
                      fillGraph = FALSE, 
                      ...) {
  
  # build options list
  options <- list()
  options$drawGrid = drawGrid
  options$fillGraph = fillGraph
  
  # add any extra parameters
  options <- mergeLists(options, list(...))
  
  # merge and return
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}

#' @export
dygraphOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "dygraphs", width, height)
}

#' @export
renderDygraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, dygraphOutput, env, quoted = TRUE)
}
