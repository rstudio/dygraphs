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
#' @export
dyUnzoom <- function(dygraph) {
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
#' @export
dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("plugins/crosshair.js", package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

#' dyRibbon plugin adds a horizontal band of colors that runs through the chart. It
#' can be useful to visualize categorical variables
#' (http://en.wikipedia.org/wiki/Categorical_variable) that change over time (along
#' the x-axis).
#'
#' @param dygraph Dygraph to add plugin to
#' @param data Vector of numeric values in the range from 0 to 1
#' @param palette Vector with colors palette
#' @param parser JavaScrip function (function (data, dygraph)) returning the array of
#' numeric values. Parser is used if no data was provided
#' @param top Vertical position of the top edge of ribbon relative to chart height.
#' @param bottom Vertical position of the bottom edge of ribbon relative to chart height.
#'
#' @examples
#' \dontrun{
#' library(quantmod)
#'
#' getSymbols("SPY", from = "2016-12-01", auto.assign = TRUE)
#'
#' difference <- SPY[, "SPY.Open"] - SPY[, "SPY.Close"]
#' decreasing <- which(difference < 0)
#' increasing <- which(difference > 0)
#'
#' dyData <- SPY[, "SPY.Close"]
#'
#' ribbonData <- rep(0, nrow(dyData))
#' ribbonData[decreasing] <- 0.5
#' ribbonData[increasing] <- 1
#'
#' dygraph(dyData) %>%
#'   dyRibbon(data = ribbonData, top = 0.1, bottom = 0.02)
#' }
#'
#' @export
dyRibbon <- function(dygraph, data = NULL, palette = NULL, parser = NULL, top = 1, bottom = 0) {
  dyPlugin(
    dygraph = dygraph,
    name = "Ribbon",
    path = system.file("plugins/ribbon.js", package = "dygraphs"),
    options = list(
      data = data,
      parser = JS(parser),
      options = list(
        palette = palette,
        top = top,
        bottom = bottom
      )
    )
  )
}
