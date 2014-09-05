
#' dygraph options
#' 
#' Add options to a dygraph plot.
#' 
#' @param fillGraph Should the area underneath the graph be filled? This option
#'   is not compatible with error bars. This option can also be set on a per-series
#'   basis via \code{\link{dySeries}}.
#' @param ... Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @export
dyOptions <- function(fillGraph = FALSE, ...) {
  options <- list()
  options$fillGraph <- fillGraph
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