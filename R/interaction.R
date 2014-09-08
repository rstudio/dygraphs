
#' Series mouse-over highlighting
#' 
#' Configure options for data series mouse-over highlighting. Note that 
#' highlighting is always enabled for dygraphs so this function is used
#' to customize rather than enable highlighting.
#' 
#' @param dygraph Dygraph to configure highlighting behavior for.
#' @param showLabelsOnHighlight Whether to show the legend upon mouseover.
#' @param hideOverlayOnMouseOut Whether to hide the legend when the mouse leaves
#'   the chart area.
#' @param highlightCircleSize The size in pixels of the dot drawn over 
#'   highlighted points.
#' @param highlightSeriesBackgroundAlpha Fade the background while highlighting 
#'   series. 1=fully visible background (disable fading), 0=hiddden background 
#'   (show highlighted series only).
#' @param highlightSeriesOpts When set, the options from this list are applied
#'   to the series closest to the mouse pointer for interactive
#'   highlighting. Example: list(strokeWidth = 3). See the documentation on 
#'   \code{\link{dySeries}} for additional details on options that can be set.
#'   
#' @return A dygraph with customized highlighting options
#'   
#' @export
dyHighlight <- function(dygraph,
                        showLabelsOnHighlight = TRUE,
                        hideOverlyOnMouseOut = TRUE,
                        highlightCircleSize = 3,
                        highlightSeriesBackgroundAlpha = 0.5,
                        highlightSeriesOpts = list()) {
  highlight <- list()
  highlight$showLabelsOnHighlight <- showLabelsOnHighlight
  highlight$hideOverlayOnMouseOut <- hideOverlyOnMouseOut
  highlight$highlightCircleSize <- highlightCircleSize
  highlight$highlightSeriesBackgroundAlpha <- highlightSeriesBackgroundAlpha
  highlight$highlightSeriesOpts <- highlightSeriesOpts
  
  # merge highlight
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, highlight)
  
  # return modified dygraph
  dygraph
}

#' Rolling average period text box
#' 
#' Add a rolling average period text box to the bottom left of the plot. Y
#' values are averaged over the specified number of time scale units.
#' 
#' @param dygraph Dygraph to add roller to
#' @param rollPeriod Number of time scale units (e.g. days, months, years) to
#'   average values over.
#'   
#' @return A dygraph that displays a range selector
#' 
#' @export
dyRoller <- function(dygraph,
                     rollPeriod = 1) {
  
  roller <- list()
  roller$showRoller <- TRUE
  roller$rollPeriod = rollPeriod
  
  # merge roller
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, roller)
  
  # return modified dygraph
  dygraph
}

#' Interactive selection and zooming of date ranges
#' 
#' Add a range selector to the bottom of the chart that allows users to pan and 
#' zoom to various date ranges.
#' 
#' @param dygraph Dygraph to add range selector to
#' @param dateWindow Initially zoom in on a section of the graph. Is a two 
#'   element vector [earliest, latest], where earliest/latest objects 
#'   convertable via \code{as.POSIXct}.
#' @param height Height, in pixels, of the range selector widget. This option 
#'   can only be specified at Dygraph creation time.
#' @param fillColor The range selector mini plot fill color. This can be of the 
#'   form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify 
#'   \code{NULL} or "" to turn off fill.
#' @param strokeColor The range selector mini plot stroke color. This can be of
#'   the form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify
#'   \code{NULL} or "" to turn off stroke.
#'   
#' @return A dygraph that displays a range selector
#'   
#' @export
dyRangeSelector <- function(dygraph,
                            dateWindow = NULL, 
                            height = 40,
                            fillColor = " #A7B1C4",
                            strokeColor = "#808FAB") {
  
  selector <- list()
  selector$showRangeSelector = TRUE
  if (!is.null(dateWindow)) {
    if (length(dateWindow) != 2)
      stop("dateWindow must be vector of length 2 that is convertible to POSIXct")
    selector$dateWindow <- sapply(USE.NAMES = FALSE, dateWindow, 
                                     function(x) { 
                                       as.double(as.POSIXct(x, tz = "GMT")) * 1000; 
                                     }
    );
  }
  selector$height <- height
  selector$rangeSelectorPlotFillColor <- fillColor
  selector$rangeSelectorPlotStrokeColor <- strokeColor
  
  # merge selector
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, selector)
  
  # return modified dygraph
  dygraph
  
}

