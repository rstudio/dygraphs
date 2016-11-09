#' dygraph series mouse-over highlighting
#' 
#' Configure options for data series mouse-over highlighting. Note that 
#' highlighting is always enabled for dygraphs so this function is used to 
#' customize rather than enable highlighting.
#' 
#' @param dygraph Dygraph to configure highlighting behavior for.
#' @param highlightCircleSize The size in pixels of the dot drawn over 
#'   highlighted points.
#' @param highlightSeriesBackgroundAlpha Fade the background while highlighting 
#'   series. 1=fully visible background (disable fading), 0=hidden background 
#'   (show highlighted series only).
#' @param highlightSeriesOpts When set, the options from this list are applied 
#'   to the series closest to the mouse pointer for interactive highlighting. 
#'   Example: list(strokeWidth = 3). See the documentation on 
#'   \code{\link{dySeries}} for additional details on options that can be set.
#' @param hideOnMouseOut Whether to hide the highlighting effects when the mouse
#'   leaves the chart area. Note that this also affects the hiding of the 
#'   \code{\link{dyLegend}} on mouse out.
#'   
#' @return A dygraph with customized highlighting options
#'   
#' @examples 
#' library(dygraphs)
#' lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
#' dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
#'   dyHighlight(highlightCircleSize = 5, 
#'               highlightSeriesBackgroundAlpha = 0.2,
#'               hideOnMouseOut = FALSE)   
#'  
#' @note See the
#' \href{https://rstudio.github.io/dygraphs/gallery-series-highlighting.html}{online
#' documentation} for additional details and examples.
#' 
#' @export
dyHighlight <- function(dygraph,
                        highlightCircleSize = 3,
                        highlightSeriesBackgroundAlpha = 0.5,
                        highlightSeriesOpts = list(),
                        hideOnMouseOut = TRUE) {
  
  highlight <- list()
  highlight$highlightCircleSize <- highlightCircleSize
  highlight$highlightSeriesBackgroundAlpha <- highlightSeriesBackgroundAlpha
  highlight$highlightSeriesOpts <- highlightSeriesOpts
  highlight$hideOverlayOnMouseOut <- hideOnMouseOut
  
  # merge highlight
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, highlight)
  
  # return modified dygraph
  dygraph
}
