#' Rebase data handler for straw broom charts with Dygraph
#'
#' Draw a straw broom chart.
#'
#' @param dygraph Dygraph to draw chart on
#' @param value Value to rebase to
#' @param percent If true, ingnore value argument and rebase to percentage difference
#'
#' @return Dygraph with specified straw broom chart
#'
#' @examples
#' library(quantmod)
#' tickers <- c("AAPL", "MSFT")
#' getSymbols(tickers)
#' closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
#' dateWindow <- c("2008-01-01", "2009-01-01")
#' dygraph(closePrices, main = "Value", group = "stock") %>%
#'   dyRebase(value = 100) %>%
#'   dyRangeSelector(dateWindow = dateWindow)
#' dygraph(closePrices, main = "Percent", group = "stock") %>%
#'   dyRebase(percent = TRUE) %>%
#'   dyRangeSelector(dateWindow = dateWindow)
#' dygraph(closePrices, main = "None", group = "stock") %>%
#'   dyRangeSelector(dateWindow = dateWindow)
#'
#' @export
dyRebase <- function(dygraph, value = 100, percent = FALSE) {
  if (percent) {
    base <- "percent"
  } else if (is.numeric(value)) {
    base <- value
  } else {
    stop("value must be of numeric type", call. = FALSE)
  }

  dyPlugin(dygraph = dygraph,
           name = "Rebase",
           path = system.file("plugins/rebase.js", package = "dygraphs"),
           options = base)
}
