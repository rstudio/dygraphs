
#' dygraph options
#' 
#' Add options to a dygraph plot.
#' 
#' @param fillGraph Should the area underneath the graph be filled? This option 
#'   is not compatible with error bars. This option can also be set on a 
#'   per-series basis via \code{\link{dySeries}}.
#' @param drawPoints Draw a small dot at each point, in addition to a line going
#'   through the point. This makes the individual data points easier to see, but
#'   can increase visual clutter in the chart. This option can also be set on a
#'   per-series basis via \code{\link{dySeries}}.
#' @param pointSize The size of the dot to draw on each point in pixels. A dot 
#'   is always drawn when a point is "isolated", i.e. there is a missing point 
#'   on either side of it. This also controls the size of those dots. This 
#'   option can also be set on a per-series basis via \code{\link{dySeries}}.
#' @param ... Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @export
dyOptions <- function(fillGraph = FALSE,
                      drawPoints = FALSE,
                      pointSize = 1,
                      ...) {
  options <- list()
  options$fillGraph <- fillGraph
  options$drawPoints <- drawPoints
  options$pointSize <- pointSize
  options <- append(options, list(...))
  structure(options, class = "dygraph.options")
}


addOptions <- function (attrs, options) {
  
  for (o in options) {
    # validate
    if (!inherits(o, "dygraph.options"))
      stop("You must pass only dyOptions objects in the options parameter")
    
    # merge 
    attrs <- mergeLists(attrs, o)
  }
  
  attrs
}