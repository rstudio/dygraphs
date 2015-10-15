#' dygraph axis
#' 
#' Define options for an axis on a dygraph plot. Note that options will use the 
#' default global setting (as determined by \code{\link{dyOptions}}) when not 
#' specified explicitly.
#' 
#' @inheritParams dyOptions
#'   
#' @param dygraph Dygraph to add an axis definition to
#' @param name Axis name ('x', 'y', or 'y2')
#' @param label Label to display for axis (defaults to none).
#' @param valueRange Explicitly set the vertical range of the graph to 
#'   \code{c(low, high)}. This may be set on a per-axis basis to define each 
#'   y-axis separately. If either limit is unspecified, it will be calculated 
#'   automatically (e.g. \code{c(NULL, 30)} to automatically calculate just the 
#'   lower bound).
#' @param ticker This lets you specify an arbitrary JavaScript function to 
#'   generate tick marks on an axis. The tick marks are an array of (value, 
#'   label) pairs. The built-in functions go to great lengths to choose good 
#'   tick marks so, if you set this option, you'll most likely want to call one 
#'   of them and modify the result. See dygraph-tickers.js and the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#' @param rangePad Add the specified amount of extra space (in pixels) around 
#'   the value range to ensure points at the edges remain visible.
#' @param labelWidth Width of the div which contains the y-axis label. Since the
#'   y-axis label appears rotated 90 degrees, this actually affects the height 
#'   of its div.
#' @param labelHeight Height of the x-axis label, in pixels. This also controls 
#'   the default font size of the x-axis label. If you style the label on your 
#'   own, this controls how much space is set aside below the chart for the 
#'   x-axis label's div.
#' @param axisHeight Height, in pixels, of the x-axis. If not set explicitly, 
#'   this is computed based on \code{axisLabelFontSize} and \code{axisTickSize}.
#' @param axisLineColor Color of the x- and y-axis lines. Accepts any value 
#'   which the HTML canvas strokeStyle attribute understands, e.g. 'black' or 
#'   'rgb(0, 100, 255)'.
#' @param axisLineWidth Thickness (in pixels) of the x- and y-axis lines.
#' @param axisLabelColor Color for x- and y-axis labels. This is a CSS color 
#'   string. This may also be set globally using \code{dyOptions}.
#' @param axisLabelFontSize Size of the font (in pixels) to use in the axis 
#'   labels, both x- and y-axis. This may also be set globally using 
#'   \code{dyOptions}.
#' @param axisLabelWidth Width, in pixels, of the axis labels
#' @param axisLabelFormatter JavaScript function to call to format the tick 
#'   values that appear along an axis (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#' @param valueFormatter JavaScript function to call to provide a custom display
#'   format for the values displayed on mouseover (see the
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#' @param pixelsPerLabel Number of pixels to require between each x- and 
#'   y-label. Larger values will yield a sparser axis with fewer ticks. Defaults
#'   to 50 (x-axis) or 30 (y-axes).
#' @param drawGrid Whether to display grid lines in the chart.
#' @param gridLineColor The color of the grid lines.
#' @param gridLineWidth Thickness (in pixels) of the grid lines drawn under the 
#'   chart.
#' @param independentTicks Only valid for y and y2, has no effect on x: This 
#'   option defines whether the y axes should align their ticks or if they 
#'   should be independent. Possible combinations: 1.) y=true, y2=false 
#'   (default): y is the primary axis and the y2 ticks are aligned to the the 
#'   ones of y. (only 1 grid) 2.) y=false, y2=true: y2 is the primary axis and 
#'   the y ticks are aligned to the the ones of y2. (only 1 grid) 3.) y=true, 
#'   y2=true: Both axis are independent and have their own ticks. (2 grids) 4.) 
#'   y=false, y2=false: Invalid configuration causes an error.
#'   
#' @return Axis options
#'   
#' @note See the 
#'   \href{http://rstudio.github.io/dygraphs/gallery-axis-options.html}{online 
#'   documentation} for additional details and examples.
#'  
#' @examples
#' library(dygraphs)
#' 
#' dygraph(nhtemp, main = "New Haven Temperatures") %>%
#'   dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
#'   dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
#'     
#' @export
dyAxis <- function(dygraph,
                   name, 
                   label = NULL, 
                   valueRange = NULL,
                   ticker = NULL,
                   rangePad = NULL,
                   labelWidth = NULL,
                   labelHeight = NULL,
                   axisHeight = NULL,
                   axisLineColor = NULL,
                   axisLineWidth = NULL,
                   pixelsPerLabel = NULL,
                   axisLabelColor = NULL,
                   axisLabelFontSize = NULL,
                   axisLabelWidth = NULL,
                   axisLabelFormatter = NULL,
                   valueFormatter = NULL,
                   drawGrid = NULL,
                   gridLineColor = NULL,
                   gridLineWidth = NULL,
                   independentTicks = NULL) {
  
  if (!name %in% c("x", "y", "y2"))
    stop("Axis name must be 'x', 'y', or 'y2'")
  
  # copy attrs for modification
  attrs <- dygraph$x$attrs
  
  axis <- list()
  axis$name <- name
  axis$label <- label
  axis$options <- list()
  axis$options$valueRange <- valueRange
   
  axis$options$ticker <- JS(ticker)
  if (!is.null(axisHeight)) {
    if (name == "x")
      attrs$xAxisHeight <- axisHeight
    else
      stop("axisHeight is only applicable to the x axis")
  }
  
  if (!is.null(rangePad))
    attrs[[sprintf("%sRangePad", axis$name)]] <- rangePad
  
  if (!is.null(labelWidth))
    attrs[[sprintf("%sLabelWidth", axis$name)]] <- labelWidth
  if (!is.null(labelHeight))
    attrs[[sprintf("%sLabelHeight", axis$name)]] <- labelHeight
  
  axis$options$axisLineColor <- axisLineColor
  axis$options$axisLineWidth <- axisLineWidth
  if (!is.null(pixelsPerLabel))
    axis$options$pixelsPerLabel <- pixelsPerLabel 
  axis$options$axisLabelColor <- axisLabelColor
  axis$options$axisLabelFontSize <- axisLabelFontSize
  
  if (!is.null(axisLabelWidth))
    axis$options$axisLabelWidth <- axisLabelWidth
  
  axis$options$axisLabelFormatter <- JS(axisLabelFormatter)
  axis$options$valueFormatter <- JS(valueFormatter)
  axis$options$drawGrid <- drawGrid
  axis$options$gridLineColor <- gridLineColor
  axis$options$gridLineWidth <- gridLineWidth
  axis$options$independentTicks <- independentTicks
 
  # label and options
  if (!is.null(axis$label))
    attrs[[sprintf("%slabel", axis$name)]] <- axis$label
  attrs$axes[[axis$name]] <- axis$options  
  
  # return modified dygraph
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, attrs)
  dygraph
}

