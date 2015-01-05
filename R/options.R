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
#' @param drawGapEdgePoints Draw points at the edges of gaps in the data. This 
#'   improves visibility of small data segments or other data irregularities.
#' @param connectSeparatedPoints Usually, when dygraphs encounters a missing 
#'   value in a data series, it interprets this as a gap and draws it as such. 
#'   If, instead, the missing values represents an x-value for which only a 
#'   different series has data, then you'll want to connect the dots by setting 
#'   this to true.
#' @param strokeWidth The width of the lines connecting data points. This can be
#'   used to increase the contrast or some graphs. This option can also be set 
#'   on a per-series basis.
#' @param strokePattern A custom pattern array where the even index is a draw 
#'   and odd is a space in pixels. If null then it draws a solid line. The array
#'   should have a even length as any odd length array could be expressed as a 
#'   smaller even length array. This is used to create dashed lines. This option
#'   can also be set on a per-series basis.
#' @param strokeBorderWidth Draw a border around graph lines to make crossing 
#'   lines more easily distinguishable. Useful for graphs with many lines. This 
#'   option can also be set on a per-series basis.
#' @param strokeBorderColor Color for the line border used if 
#'   \code{strokeBorderWidth} is set. This option can also be set on a 
#'   per-series basis.
#' @param plotter A function (or array of functions) which plot each data series
#'   on the chart. May also be set on a per-series basis. See the 
#'   \href{http://dygraphs.com/tests/plotters.html}{dygraphs documentation} for 
#'   additional details on plotting functions.
#' @param colors Character vector of colors for the data series. These can be of
#'   the form "#AABBCC" or "rgb(255,100,200)" or "yellow", etc. If not 
#'   specified, equally-spaced points around a color wheel are used. This option
#'   can also be set on a per-series basis. Note that in both global and 
#'   per-series specification of custom colors you must provide a color for all 
#'   series being displayed. Note also that global and per-series color 
#'   specification cannot be mixed.
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
#' @param drawAxesAtZero When set, draw the X axis at the Y=0 position and the Y
#'   axis at the X=0 position if those positions are inside the graph's visible 
#'   area. Otherwise, draw the axes at the bottom or left graph edge as usual.
#' @param logscale When set the graph shows the y-axis in log scale. Any values 
#'   less than or equal to zero are not displayed.
#' @param axisTickSize The spacing between axis labels and tick marks.
#' @param axisLineColor Color of the x- and y-axis lines. Accepts any value 
#'   which the HTML canvas strokeStyle attribute understands, e.g. 'black' or 
#'   'rgb(0, 100, 255)'. This can also be set on a per-axis basis.
#' @param axisLineWidth Thickness (in pixels) of the x- and y-axis lines. This 
#'   can also be set on a per-axis basis.
#' @param axisLabelColor Color for x- and y-axis labels. This is a CSS color 
#'   string. This may also be set on a per-axis basis.
#' @param axisLabelFontSize Size of the font (in pixels) to use in the axis 
#'   labels, both x- and y-axis. This may also be set on a per-axis basis.
#' @param axisLabelWidth Width (in pixels) of the containing divs for x- and 
#'   y-axis labels.
#' @param drawGrid Whether to display grid lines in the chart. This may be set 
#'   on a per-axis basis to define the visibility of each axis' grid separately.
#'   Defaults to \code{TRUE} for x and y, and \code{FALSE} for y2.
#' @param gridLineColor The color of the grid lines. This option can also be set
#'   on a per-series basis.
#' @param gridLineWidth Thickness (in pixels) of the grid lines drawn under the 
#'   chart. This option can also be set on a per-series basis.
#' @param titleHeight Height of the chart title, in pixels. This also controls 
#'   the default font size of the title. If you style the title on your own, 
#'   this controls how much space is set aside above the chart for the title's 
#'   div.
#' @param rightGap Number of pixels to leave blank at the right edge of the 
#'   Dygraph. This makes it easier to highlight the right-most data point.
#' @param digitsAfterDecimal Unless it's run in scientific mode (see the 
#'   \code{sigFigs} option), dygraphs displays numbers with 
#'   \code{digitsAfterDecimal} digits after the decimal point. Trailing zeros 
#'   are not displayed, so with a value of 2 you'll get '0', '0.1', '0.12', 
#'   '123.45' but not '123.456' (it will be rounded to '123.46'). Numbers with 
#'   absolute value less than 0.1^digitsAfterDecimal (i.e. those which would 
#'   show up as '0.00') will be displayed in scientific notation.
#' @param labelsKMB Show K/M/B for thousands/millions/billions on y-axis.
#' @param labelsKMG2 Show k/M/G for kilo/Mega/Giga on y-axis. This is different 
#'   than \code{labelsKMB} in that it uses base 2, not 10.
#' @param maxNumberWidth When displaying numbers in normal (not scientific) 
#'   mode, large numbers will be displayed with many trailing zeros (e.g. 
#'   100000000 instead of 1e9). This can lead to unwieldy y-axis labels. If 
#'   there are more than maxNumberWidth digits to the left of the decimal in a 
#'   number, dygraphs will switch to scientific notation, even when not 
#'   operating in scientific mode. If you'd like to see all those digits, set 
#'   this to something large, like 20 or 30.
#' @param sigFigs By default, dygraphs displays numbers with a fixed number of 
#'   digits after the decimal point. If you'd prefer to have a fixed number of 
#'   significant figures, set this option to that number of significant figures.
#'   A value of 2, for instance, would cause 1 to be display as 1.0 and 1234 to
#'   be displayed as 1.23e+3.
#' @param panEdgeFraction A value representing the farthest a graph may be 
#'   panned, in percent of the display. For example, a value of 0.1 means that 
#'   the graph can only be panned 10% pased the edges of the displayed values. 
#'   null means no bounds.
#' @param animatedZooms Set this option to animate the transition between zoom 
#'   windows. Applies to programmatic and interactive zooms. Note that if you 
#'   also set a drawCallback, it will be called several times on each zoom. If 
#'   you set a zoomCallback, it will only be called after the animation is 
#'   complete.
#' @param timingName Set this option to log timing information. The value of the
#'   option will be logged along with the timing, so that you can distinguish 
#'   multiple dygraphs on the same page.
#' @param useDataTimezone Whether to use the time zone of the underlying xts
#'  object for display. Defaults to \code{FALSE} which uses the time zone
#'  of the client workstation.
#' @return dygraph with additional options
#'   
#' @note See the \href{http://rstudio.github.io/dygraphs/}{online documentation}
#'   for additional details and examples.
#'   
#' @export
dyOptions <- function(dygraph,
                      stackedGraph = FALSE,
                      fillGraph = FALSE,
                      fillAlpha = 0.15,
                      stepPlot = FALSE,
                      drawPoints = FALSE,
                      pointSize = 1.0,
                      drawGapEdgePoints = FALSE,
                      connectSeparatedPoints = FALSE,
                      strokeWidth = 1.0,
                      strokePattern = NULL,
                      strokeBorderWidth = NULL,
                      strokeBorderColor = "white",
                      plotter = NULL,
                      colors = NULL,
                      colorValue = 0.5,
                      colorSaturation = 1.0,
                      drawXAxis = TRUE,
                      drawYAxis = TRUE,
                      includeZero = FALSE,
                      drawAxesAtZero = FALSE,
                      logscale = FALSE,
                      axisTickSize = 3.0,
                      axisLineColor = "black",
                      axisLineWidth = 0.3,
                      axisLabelColor = "black",
                      axisLabelFontSize = 14,
                      axisLabelWidth = 50,
                      drawGrid = TRUE,
                      gridLineColor = NULL,
                      gridLineWidth = 0.3,
                      titleHeight = NULL,
                      rightGap = 5,
                      digitsAfterDecimal = 2,
                      labelsKMB = FALSE,
                      labelsKMG2 = FALSE,
                      maxNumberWidth = 6,
                      sigFigs = NULL,
                      panEdgeFraction = NULL,
                      animatedZooms = FALSE,
                      timingName = NULL,
                      useDataTimezone = FALSE) {
  options <- list()
  options$stackedGraph <- stackedGraph
  options$fillGraph <- fillGraph
  options$fillAlpha = fillAlpha
  options$stepPlot <- stepPlot
  options$drawPoints <- drawPoints
  options$pointSize <- pointSize
  options$drawGapEdgePoints <- drawGapEdgePoints
  options$connectSeparatedPoints <- connectSeparatedPoints
  options$strokeWidth <- strokeWidth
  options$strokePattern <- strokePattern
  options$strokeBorderWidth <- strokeBorderWidth
  options$strokeBorderColor <- strokeBorderColor
  options$plotter <- JS(plotter)
  options$colors <- colors
  options$colorValue <- colorValue
  options$colorSaturation <- colorSaturation
  options$drawXAxis <- drawXAxis
  options$drawYAxis <- drawYAxis
  options$includeZero <- includeZero
  options$drawAxesAtZero <- drawAxesAtZero
  options$logscale <- logscale
  options$axisTickSize <- axisTickSize
  options$axisLineColor <- axisLineColor
  options$axisLineWidth <- axisLineWidth
  options$axisLabelColor <- axisLabelColor
  options$axisLabelFontSize <- axisLabelFontSize
  options$axisLabelWidth <- axisLabelWidth
  options$drawGrid <- drawGrid
  options$gridLineColor <- gridLineColor
  options$gridLineWidth <- gridLineWidth
  options$titleHeight <- titleHeight
  options$rightGap <- rightGap
  options$digitsAfterDecimal <- digitsAfterDecimal
  options$labelsKMB <- labelsKMB
  options$labelsKMG2 <- labelsKMG2
  options$maxNumberWidth <- maxNumberWidth
  options$sigFigs <- sigFigs
  options$panEdgeFraction <- panEdgeFraction
  options$animatedZooms <- animatedZooms
  options$timingName <- timingName
  
  # merge options into attrs
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, options)
   
  # use data timezone
  data.timezone <- attr(attr(dygraph$x, "time"),"tzone")
  if (is.null(data.timezone)) {
    data.timezone <- ""
  }
  dygraph$x$fixedtz <- useDataTimezone
  if (useDataTimezone && (data.timezone == "")) {
    warning("Can't use data time zone: no 'tzone' attribute in data")
    dygraph$x$fixedtz <- FALSE
  }
  dygraph$x$tzone <- data.timezone
  
  # return modified dygraph
  dygraph
}
