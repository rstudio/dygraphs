
#' dygraph interaction
#' 
#' Add interactive behaviors to a dygraph plot.
#' 
#' @param dygraph Dygraph to add interactive behaviors to
#' @param highlightCircleSize The size in pixels of the dot drawn over 
#'   highlighted points.
#' @param highlightSeriesBackgroundAlpha Fade the background while highlighting 
#'   series. 1=fully visible background (disable fading), 0=hiddden background 
#'   (show highlighted series only).
#' @param highlightSeriesOpts When set, the options from this list are applied
#'   to the series closest to the mouse pointer for interactive
#'   highlighting. Example: list(strokeWidth = 3). See the documentation on 
#'   \code{\link{dySeries}} for additional details on options that can be set.
#' @param showLabelsOnHighlight Whether to show the legend upon mouseover.
#' @param hideOverlayOnMouseOut Whether to hide the legend when the mouse leaves
#'   the chart area.
#' @param showRoller If the rolling average period text box should be shown.
#' @param rollPeriod Number of timestamps over which to average data.
#'   
#' @return Interaction options
#'   
#' @export
dyInteraction <- function(dygraph,
                          highlightCircleSize = 3,
                          highlightSeriesBackgroundAlpha = 0.5,
                          highlightSeriesOpts = list(),
                          showLabelsOnHighlight = TRUE,
                          hideOverlyOnMouseOut = TRUE,
                          showRoller = FALSE,
                          rollPeriod = 1) {
  interaction <- list()
  interaction$highlightCircleSize <- highlightCircleSize
  interaction$highlightSeriesBackgroundAlpha <- highlightSeriesBackgroundAlpha
  interaction$highlightSeriesOpts <- highlightSeriesOpts
  interaction$showLabelsOnHighlight <- showLabelsOnHighlight
  interaction$hideOverlayOnMouseOut <- hideOverlyOnMouseOut
  interaction$showRoller <- showRoller
  interaction$rollPeriod = rollPeriod
  
  # merge interactions
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, interaction)
  
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

