
# TODO: resolve all functions

# TODO: docs and examples

#' Interactive plot for time series data
#' 
#' R interface to interactive time series plotting using the 
#' \href{http://dygraphs.com}{dygraphs} JavaScript library.
#' 
#' @param data Time series data (must be an \link[xts]{xts} object or an object 
#'   which is covertible to \code{xts}).
#' @param title Main plot title (optional)
#' @param series Series definition (or list of series definitions) created using
#'   the \code{\link{dySeries}} function. Series can be bound positionally or 
#'   explicity using the \code{name} parameter of \code{dySeries}.
#' @param axes Axis definition (or list of axis definitions) created using the 
#'   the \code{\link{dyAxis}} function.
#' @param options Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#' @param group Group to associate this plot with. The x-axis zoom level of 
#'   plots within a group is automatically synchronized.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#'   
#' @return Interactive dygraph plot
#'   
#' @export
dygraph <- function(data, 
                    title = NULL,
                    series = list(),
                    axes = list(),
                    options = list(),
                    group = NULL,
                    width = NULL, 
                    height = NULL) {
  
  # allow series and axes to be specified as single elements
  if (inherits(series, "dygraph.series"))
    series <- list(series)
  if (inherits(axes, "dygraph.axis"))
    axes <- list(axes)
  
  # convert data to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity 
  periodicity <- periodicity(data)
  
  # convert time string we can pass to javascript Date function and
  # extract core data from xts object
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  data <- zoo::coredata(data)
      
  # convert to a named list
  data <- unclass(as.data.frame(data))
   
  # resolve custom series (aggregates or smoothed versions of input series)
  customBars <- haveCustomBars(series)
  if (customBars) {
    resolved <- resolveCustomBars(data, series)
    data <- resolved$data
    series <- resolved$series
  }
  
  # get column names, merge time into data-frame, then strip the metadata 
  # (so we are marshalled as a 2d array into json)
  colNames <- names(data)
  data <- append(list(time), data)
  names(data) <- NULL
    
  # create native dygraph options object
  x <- list()
  x$title <- title
  x$customBars <- customBars
  x$labels <- c(periodicity$label, colNames)
  if (length(colNames) > 1)
    x$legend <- "always"
  x$axes$x <- list() 
   
  # add series, axes, and options
  x <- addSeries(x, series)
  x <- addAxes(x, axes)
  x <- mergeLists(x, options)
  
  # side data we use in javascript
  meta <- list()
  meta$scale <- periodicity$scale
  meta$group <- group
  x$meta <- meta
  
  # add time series data (we do this at the end so we don't pay the
  # price of copying it as we mutate 'x' above)
  x$file <- data
  
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
dygraphOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "dygraphs", width, height)
}

#' @export
renderDygraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, dygraphOutput, env, quoted = TRUE)
}
