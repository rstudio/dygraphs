#' Candlestick plotter for dygraph chart
#'
#' Draw a candlestick chart.
#'
#' @param dygraph Dygraph to draw chart on
#'
#' @return Dygraph with specified candlestick plotter
#'
#' @examples
#' library(xts)
#' data(sample_matrix)
#' library(dygraphs)
#' dygraph(sample_matrix) %>%
#'   dyCandlestick()
#'
#' @export
dyCandlestick <- function(dygraph) {
  path <- system.file("plugins/candlestick.js", package = "dygraphs")
  path <- normalizePath(path)
  dyPlotter(dygraph = dygraph,
            name = "CandlestickPlotter",
            path = path,
            version = "1.0")
}
