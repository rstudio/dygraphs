#' dygraph rolling average period text box
#'
#' Add a rolling average period text box to the bottom left of the plot. Y
#' values are averaged over the specified number of time scale units.
#'
#' @param dygraph Dygraph to add roller to
#' @param showRoller Whether to show the roller
#' @param rollPeriod Number of time scale units (e.g. days, months, years) to
#'   average values over.
#'
#' @return A dygraph that displays a range selector
#'
#' @examples
#' library(dygraphs)
#'
#' dygraph(discoveries, main = "Important Discoveries") %>%
#'   dyRoller(rollPeriod = 5)
#' @note See the
#' \href{https://rstudio.github.io/dygraphs/gallery-roll-periods.html}{online
#' documentation} for additional details and examples.
#'
#' @export
dyRoller <- function(dygraph,
                     showRoller = TRUE,
                     rollPeriod = 1) {
  roller <- list()
  roller$showRoller <- showRoller
  roller$rollPeriod <- rollPeriod

  # merge roller
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, roller)

  # return modified dygraph
  dygraph
}
