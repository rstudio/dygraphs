#' The dyUnzoom plugin adds an "Unzoom" button to the graph when it's displaying
#' in a zoomed state (this is a bit more discoverable than the default double-
#' click gesture for unzooming).
#'
#' @param dygraph Dygraph to add plugin to
#'
#' @return Dygraph with Unzoom plugin enabled
#'
#' @examples
#' library(dygraphs)
#' dygraph(ldeaths) %>%
#'   dyRangeSelector() %>%
#'   dyUnzoom()
#'
#' @export
dyUnzoom <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("plugins/unzoom.js", package = "dygraphs")
  )
}

#' The dyCrosshair plugin draws a crosshair line over the point closest to the
#' mouse when the user hovers over the graph. It has a "direction" option which
#' is provided in the R wrapper function and then forwarded to the plugin using
#' the "options" argument to dyPlugin.
#'
#' @param dygraph Dygraph to add plugin to
#' @param direction Crosshair direction. Valid options are: "both", "horizontal",
#' "vertical"
#'
#' @return Dygraph with Crosshair plugin enabled
#'
#' @examples
#' library(dygraphs)
#' dygraph(ldeaths) %>%
#'   dyRangeSelector() %>%
#'   dyCrosshair(direction = "vertical")
#'
#' @export
dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("plugins/crosshair.js", package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}
