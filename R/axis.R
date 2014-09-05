
#' dygraph axis options
#' 
#' Define options for an axis on a dygraph plot. Note that options will use the 
#' default global setting (as determined by \code{\link{dyOptions}}) when not 
#' specified explicitly.
#' 
#' @inheritParams dyOptions
#'   
#' @param name Axis name ('x', 'y', or 'y2')
#' @param label Label to display for axis (defaults to none)
#' @param drawGrid Whether to display gridlines in the chart.
#' @param gridLineColor The color of the gridlines.
#' @param gridLineWidth Thickness (in pixels) of the gridlines drawn under the
#'   chart.=
#' @param independentTicks Only valid for y and y2, has no effect on x: This 
#'   option defines whether the y axes should align their ticks or if they 
#'   should be independent. Possible combinations: 1.) y=true, y2=false 
#'   (default): y is the primary axis and the y2 ticks are aligned to the the 
#'   ones of y. (only 1 grid) 2.) y=false, y2=true: y2 is the primary axis and 
#'   the y ticks are aligned to the the ones of y2. (only 1 grid) 3.) y=true, 
#'   y2=true: Both axis are independent and have their own ticks. (2 grids) 4.) 
#'   y=false, y2=false: Invalid configuration causes an error.
#' @return Axis options
#'   
#' @export
dyAxis <- function(name, 
                   label = NULL, 
                   drawGrid = NULL,
                   gridLineColor = NULL,
                   gridLineWidth = NULL,
                   independentTicks = NULL,
                   ...) {
  
  if (!name %in% c("x", "y", "y2"))
    stop("Axis name must be 'x', 'y', or 'y2'")
  
  axis <- list()
  axis$name <- name
  axis$label <- label
  axis$options <- list(...)
  axis$options$drawGrid <- drawGrid
  axis$options$gridLineColor <- gridLineColor
  axis$options$gridLineWidth <- gridLineWidth
  axis$options$independentTicks <- independentTicks
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
