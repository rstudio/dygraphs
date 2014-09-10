#' dygraph legend
#' 
#' Configure options for the dygraph series legend.
#' 
#' @param dygraph Dygraph to configure legend options for.
#' @param show When to display the legend. Specify "always" to always show the 
#'   legend. Specify "onmouseover" to only display it when a user mouses over 
#'   the chart. The default behavior is "auto", which results in "always" when
#'   more than one series is plotted and "onmouseover" when only a single series
#'   is plotted.
#' @param width Width (in pixels) of the div which shows the legend.
#' @param showZeroValues Show zero value labels in the legend.
#' @param externalDiv Show data labels in an external div, rather than on the 
#'   graph. This value should be a div element id.
#' @param hideOnMouseOut Whether to hide the legend when the mouse leaves the 
#'   chart area. This option applies when \code{show} is set to "onmouseover". 
#'   Note that this also affects the hiding of the \code{\link{dyHighlight}} 
#'   on mouse out.
#'   
#' @return A dygraph with customized legend options
#'   
#' @export
dyLegend <- function(dygraph,
                     show = c("auto", "always", "onmouseover"),
                     width = 250,
                     showZeroValues = TRUE,
                     externalDiv = NULL,
                     hideOnMouseOut = TRUE) {
  
  legend <- list()
  legend$legend <- match.arg(show)
  legend$labelsDivWidth <- width
  legend$labelsShowZeroValues <- showZeroValues
  legend$labelsDiv <- externalDiv
  legend$hideOverlayOnMouseOut <- hideOnMouseOut
   
  # merge legend
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, legend)
  
  # return modified dygraph
  dygraph
}