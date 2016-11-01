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
  name <- "candlestickPlotter"
  version <- "1.0"
  path <- system.file("plugins/candlestick.js", package = "dygraphs")
  path <- normalizePath(path)
  plotterDependency <- htmlDependency(paste0("Dygraph.Plotters", name),
                                     version,
                                     src = dirname(path),
                                     script = basename(path),
                                     all_files = FALSE)

  # add the plotter javascript to the dependencies
  if (is.null(dygraph$dependencies)) {
    dygraph$dependencies <- list()
  }
  dygraph$dependencies[[length(dygraph$dependencies) + 1]] <- plotterDependency

  dygraph$x$plotter <- name

  dygraph
}
