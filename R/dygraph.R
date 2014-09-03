 

#' @importFrom magrittr %>%
#' @export %>%
#' 
#' @export
dygraph <- function(data, title = NULL, width = NULL, height = NULL) {
  
  # convert to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity and use that for the x-axis label
  xLabel <- periodicity(data)$label
  
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
  x$labels <- c(xLabel, colNames)
 
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10, browser.fill = TRUE)
  )
}

#' @export
dyAxis <- function(dygraph, name, label = NULL, pixelsPerLabel = NULL, drawGrid = TRUE) {
  
  # axis options
  options <- list()
  options[[sprintf("%slabel", name)]] <- label
  options$axes[[name]]$pixelsPerLabel <- pixelsPerLabel
  options$axes[[name]]$drawGrid <- drawGrid
  
  # merge with main options
  dygraph$x <- mergeLists(dygraph$x, options)
  dygraph
}

#' @export
dySeries <- function(dygraph, 
                     name = NULL, 
                     label = NULL, 
                     fillGraph = FALSE, 
                     strokeWidth = 1.0, 
                     drawPoints = FALSE, 
                     pointSize = 1,
                     highlightCircleSize = 3) {
  
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
  options$strokeWidth <- strokeWidth
  options$drawPoints <- drawPoints
  options$pointSize <- pointSize
  options$highlightCircleSize <- highlightCircleSize
  dygraph$x$series[[name]] <- options
  
  # return modified dygraph
  dygraph
}

#' @export
dyRangeSelector <- function(dygraph,
                            height = 40,  
                            plotFillColor = "#A7B1C4", 
                            plotStrokeColor = "#A7B1C4") {
  selector <- list()
  selector$showRangeSelector <- TRUE
  selector$rangeSelectorHeight <- height
  selector$rangeSelectorPlotFillColor <- plotFillColor
  selector$rangeSelectorPlotStrokeColor <- plotStrokeColor
  dygraph$x <- mergeLists(dygraph$x, selector)
  dygraph
}

#' @export
dyOptions <- function(dygraph, ...) {
  dygraph$x <- mergeLists(dygraph$x, list(...))
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
