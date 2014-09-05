
#' dygraph options
#' 
#' Add options to a dygraph plot.
#' 
#' @param stackedGraph If set, stack series on top of one another rather than 
#'   drawing them independently. The first series specified in the input data 
#'   will wind up on top of the chart and the last will be on bottom.
#' @param stepPlot When set, display the graph as a step plot instead of a line 
#'   plot. This option can also be set on a per-series basis.
#' @param fillGraph Should the area underneath the graph be filled? This option 
#'   is not compatible with error bars. This option can also be set on a 
#'   per-series basis.
#' @param fillAlpha Transparency for filled regions of the plot. A value of 0.0 
#'   means that the fill will not be drawn, whereas a value of 1.0 means that 
#'   the fill will be as dark as the line of the series itself.
#' @param drawPoints Draw a small dot at each point, in addition to a line going
#'   through the point. This makes the individual data points easier to see, but
#'   can increase visual clutter in the chart. This option can also be set on a 
#'   per-series basis.
#' @param pointSize The size of the dot to draw on each point in pixels. A dot 
#'   is always drawn when a point is "isolated", i.e. there is a missing point 
#'   on either side of it. This also controls the size of those dots. This 
#'   option can also be set on a per-series basis.
#' @param strokeWidth The width of the lines connecting data points. This can be
#'   used to increase the contrast or some graphs. This option can also be set 
#'   on a per-series basis.
#' @param strokePattern A custom pattern array where the even index is a draw 
#'   and odd is a space in pixels. If null then it draws a solid line. The array
#'   should have a even length as any odd lengthed array could be expressed as a
#'   smaller even length array. This is used to create dashed lines. This option
#'   can also be set on a per-series basis.
#' @param strokeBorderWidth Draw a border around graph lines to make crossing 
#'   lines more easily distinguishable. Useful for graphs with many lines. This 
#'   option can also be set on a per-series basis.
#' @param strokeBorderColor Color for the line border used if 
#'   \code{strokeBorderWidth} is set. This option can also be set on a 
#'   per-series basis.
#' @param colorValue If custom per-series colors are not specified, value of the
#'   data series colors, as in hue/saturation/value (0.0-1.0, default 0.5).
#' @param colorSaturation If custom per-series colors are not specified, 
#'   saturation of the automatically-generated data series colors (0.0-1.0, 
#'   default 0.5).
#' @param ... Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @export
dyOptions <- function(stackedGraph = FALSE,
                      stepPlot = FALSE,
                      fillGraph = FALSE,
                      fillAlpha = 0.15,
                      drawPoints = FALSE,
                      pointSize = 1.0,
                      strokeWidth = 1.0,
                      strokePattern = NULL,
                      strokeBorderWidth = NULL,
                      strokeBorderColor = "white",
                      colorValue = 0.5,
                      colorSaturation = 1.0
                      ...) {
  options <- list()
  options$stackedGraph <- stackedGraph
  options$stepPlot <- stepPlot
  options$fillGraph <- fillGraph
  options$fillAlpha = fillAlpha
  options$drawPoints <- drawPoints
  options$pointSize <- pointSize
  options$strokeWidth <- strokeWidth
  options$strokePattern <- strokePattern
  options$strokeBorderWidth <- strokeBorderWidth
  options$strokeBorderColor <- strokeBorderColor
  options$colorValue <- colorValue
  options$colorSaturation <- colorSaturation
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