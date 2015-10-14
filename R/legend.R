#' dygraph legend
#' 
#' Configure options for the dygraph series legend.
#' 
#' @param dygraph Dygraph to configure legend options for.
#' @param show When to display the legend. Specify "always" to always show the 
#'   legend. Specify "onmouseover" to only display it when a user mouses over 
#'   the chart. Specify "follow" to have the legend show as overlay to the chart
#'   which follows the mouse. The default behavior is "auto", which results in
#'   "always" when more than one series is plotted and "onmouseover" when only a
#'   single series is plotted.
#' @param width Width (in pixels) of the div which shows the legend.
#' @param showZeroValues Show zero value labels in the legend.
#' @param labelsDiv Show data labels in an external div, rather than on the 
#'   graph. This value should be a div element id.
#' @param labelsSeparateLines Put a <br/> between lines in the label string. 
#'   Often used in conjunction with \code{labelsDiv}.
#' @param hideOnMouseOut Whether to hide the legend when the mouse leaves the 
#'   chart area. This option applies when \code{show} is set to "onmouseover". 
#'   Note that this also affects the hiding of the \code{\link{dyHighlight}} on 
#'   mouse out.
#'   
#' @return A dygraph with customized legend options
#'  
#' @examples
#' library(dygraphs)
#' 
#' dygraph(nhtemp, main = "New Haven Temperatures") %>% 
#'   dySeries("V1", label = "Temperature (F)") %>%
#'   dyLegend(show = "always", hideOnMouseOut = FALSE)   
#'     
#' @note See the 
#'   \href{http://rstudio.github.io/dygraphs/gallery-plot-legend.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dyLegend <- function(dygraph,
                     show = c("auto", "always", "onmouseover", "follow", "never"),
                     width = 250,
                     showZeroValues = TRUE,
                     labelsDiv = NULL,
                     labelsSeparateLines = FALSE,
                     hideOnMouseOut = TRUE) {
  
  legend <- list()
  legend$legend <- match.arg(show)
  if (legend$legend == "never") {
    legend$legend <- NULL
    legend$showLabelsOnHighlight <- FALSE
  }
  legend$labelsDivWidth <- width
  legend$labelsShowZeroValues <- showZeroValues
  legend$labelsDiv <- labelsDiv
  legend$labelsSeparateLines <- labelsSeparateLines
  legend$hideOverlayOnMouseOut <- hideOnMouseOut
   
  # merge legend
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, legend)
  
  # return modified dygraph
  dygraph
}