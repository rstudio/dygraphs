
#' dygraph axis options
#' 
#' Define options for an axis on a dygraph plot.
#' 
#' @param name Axis name ('x', 'y', or 'y2')
#' @param label Label to display for axis (defaults to none)
#' @param ... Per-axis options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Axis options
#'   
#' @export
dyAxis <- function(name, label = NULL, ...) {
  
  if (!name %in% c("x", "y", "y2"))
    stop("Axis name must be 'x', 'y', or 'y2'")
  
  axis <- list()
  axis$name <- name
  axis$label <- label
  axis$options <- list(...)
  structure(axis, class = "dygraph.axis")
}


addAxes <- function (attrs, axes) {

  if (length(axes) > 0) {
    for (i in 1:length(axes)) {
      
      # copy the axis and validate it
      axis <- axes[[i]]
      if (!inherits(axis, "dygraph.axis"))
        stop("You must pass only dyAxis objects in the axes parameter")
      
      # set axis options
      attrs[[sprintf("%slabel", axis$name)]] <- axis$label
      attrs$axes[[axis$name]] <- axis$options  
    }
  }
  attrs
}
