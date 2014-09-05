
#' dygraph interaction
#' 
#' Add interactive behaviors to a dygraph plot.
#' 
#' @param showRangeSelector Show or hide the range selector widget.
#' @param dateWindow Initially zoom in on a section of the graph. Is a two 
#'   element vector [earliest, latest], where earliest/latest objects 
#'   convertable via \code{as.POSIXct}.
#' @param animatedZooms Set this option to animate the transition between zoom 
#'   windows.
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
dyInteraction <- function(showRangeSelector = FALSE,
                          dateWindow = NULL,
                          animatedZooms = FALSE,
                          highlightCircleSize = 3,
                          highlightSeriesBackgroundAlpha = 0.5,
                          highlightSeriesOpts = list(),
                          showLabelsOnHighlight = TRUE,
                          hideOverlyOnMouseOut = TRUE,
                          showRoller = FALSE,
                          rollPeriod = 1) {
  interaction <- list()
  interaction$showRangeSelector <- showRangeSelector
  if (!is.null(dateWindow)) {
    if (length(dateWindow) != 2)
      stop("dateWindow must be vector of length 2 that is convertible to POSIXct")
    interaction$dateWindow <- sapply(USE.NAMES = FALSE, dateWindow, 
       function(x) { 
         as.double(as.POSIXct(x, tz = "GMT")) * 1000; 
       }
    );
  }
  interaction$animatedZooms <- animatedZooms
  interaction$highlightCircleSize <- highlightCircleSize
  interaction$highlightSeriesBackgroundAlpha <- highlightSeriesBackgroundAlpha
  interaction$highlightSeriesOpts <- highlightSeriesOpts
  interaction$showLabelsOnHighlight <- showLabelsOnHighlight
  interaction$hideOverlayOnMouseOut <- hideOverlyOnMouseOut
  interaction$showRoller <- showRoller
  interaction$rollPeriod = rollPeriod
  structure(interaction, class = "dygraph.interaction")
}


addInteraction <- function (attrs, interaction) {
  
  for (i in interaction) {
    # validate
    if (!inherits(i, "dygraph.interaction"))
      stop("You must pass only dyInteraction objects in the interaction parameter")
    
    # merge 
    attrs <- mergeLists(attrs, i)
  }
  
  attrs
}