#' dygraph options
#' 
#' Add options to a dygraph plot.
#' 
#' @param dygraph Dygraph to add options to
#' @param stackedGraph If set, stack series on top of one another rather than 
#'   drawing them independently. The first series specified in the input data 
#'   will wind up on top of the chart and the last will be on bottom.
#' @param fillGraph Should the area underneath the graph be filled? This option 
#'   is not compatible with error bars. This option can also be set on a 
#'   per-series basis.
#' @param fillAlpha Transparency for filled regions of the plot. A value of 0.0 
#'   means that the fill will not be drawn, whereas a value of 1.0 means that 
#'   the fill will be as dark as the line of the series itself.
#' @param stepPlot When set, display the graph as a step plot instead of a line 
#'   plot. This option can also be set on a per-series basis.
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
#' @param colors Character vector of colors for the data series. These can be of
#'   the form "#AABBCC" or "rgb(255,100,200)" or "yellow", etc. If not 
#'   specified, equally-spaced points around a color wheel are used. This option
#'   can also be set on a per-series basis. Note that in both global and 
#'   per-series specification of custom colors you must provide a color for all
#'   series being displayed.
#' @param colorValue If custom colors are not specified, value of the data
#'   series colors, as in hue/saturation/value (0.0-1.0, default 0.5).
#' @param colorSaturation If custom colors are not specified, saturation of the
#'   automatically-generated data series colors (0.0-1.0, default 0.5).
#' @param drawXAxis Whether to draw the x-axis. Setting this to false also 
#'   prevents x-axis ticks from being drawn and reclaims the space for the chart
#'   grid/lines.
#' @param drawYAxis Whether to draw the y-axis. Setting this to false also 
#'   prevents y-axis ticks from being drawn and reclaims the space for the chart
#'   grid/lines.
#' @param includeZero Usually, dygraphs will use the range of the data plus some
#'   padding to set the range of the y-axis. If this option is set, the y-axis 
#'   will always include zero, typically as the lowest value. This can be used 
#'   to avoid exaggerating the variance in the data.
#' @param axisLineColor Color of the x- and y-axis lines. Accepts any value 
#'   which the HTML canvas strokeStyle attribute understands, e.g. 'black' or 
#'   'rgb(0, 100, 255)'.
#' @param axisLineWidth Thickness (in pixels) of the x- and y-axis lines.
#' @param drawGrid Whether to display gridlines in the chart. This may be set on
#'   a per-axis basis to define the visibility of each axis' grid separately. 
#'   Defaults to \code{TRUE} for x and y, and \code{FALSE} for y2.
#' @param gridLineColor The color of the gridlines. This option can also be set 
#'   on a per-series basis.
#' @param gridLineWidth Thickness (in pixels) of the gridlines drawn under the 
#'   chart.This option can also be set on a per-series basis.
#' @param css Path to css file to be used for styling textual elements of the 
#'   graph. See the \href{http://dygraphs.com/css.html}{CSS documentation} on 
#'   the dygraphs website for additional details on which styles are available. 
#'   Note that CSS styles are global so will affect all dycharts on a given web 
#'   page. This also implies that for a page with multiple plots you only need 
#'   to specify styles for the first one (alternatively you can just add them 
#'   directly to the page by other means).
#' @param ... Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @note
#' See the \href{http://jjallaire.github.io/dygraphs/}{online documentation} for
#' additional details and examples.  
#'    
#' @export
dyOptions <- function(dygraph,
                      stackedGraph = FALSE,
                      fillGraph = FALSE,
                      fillAlpha = 0.15,
                      stepPlot = FALSE,
                      drawPoints = FALSE,
                      pointSize = 1.0,
                      strokeWidth = 1.0,
                      strokePattern = NULL,
                      strokeBorderWidth = NULL,
                      strokeBorderColor = "white",
                      colors = NULL,
                      colorValue = 0.5,
                      colorSaturation = 1.0,
                      drawXAxis = TRUE,
                      drawYAxis = TRUE,
                      includeZero = FALSE,
                      axisLineColor = "black",
                      axisLineWidth = 0.3,
                      drawGrid = TRUE,
                      gridLineColor = NULL,
                      gridLineWidth = 0.3,
                      css = NULL,
                      ...) {
  options <- list()
  options$stackedGraph <- stackedGraph
  options$fillGraph <- fillGraph
  options$fillAlpha = fillAlpha
  options$stepPlot <- stepPlot
  options$drawPoints <- drawPoints
  options$pointSize <- pointSize
  options$strokeWidth <- strokeWidth
  options$strokePattern <- strokePattern
  options$strokeBorderWidth <- strokeBorderWidth
  options$strokeBorderColor <- strokeBorderColor
  options$colors <- colors
  options$colorValue <- colorValue
  options$colorSaturation <- colorSaturation
  options$drawXAxis <- drawXAxis
  options$drawYAxis <- drawYAxis
  options$includeZero <- includeZero
  options$axisLineColor <- axisLineColor
  options$axisLineWidth <- axisLineWidth
  options$drawGrid < drawGrid
  options$gridLineColor <- gridLineColor
  options$gridLineWidth <- gridLineWidth
  options <- append(options, list(...))
  
  # merge options into attrs
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, options)
  
  # read css
  if (!is.null(css))
    dygraph$x$css <- paste(readLines(css, warn = FALSE), collapse = "\n")
  
  # return modified dygraph
  dygraph
}
