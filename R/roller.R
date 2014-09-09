#' Rolling average period text box
#' 
#' Add a rolling average period text box to the bottom left of the plot. Y
#' values are averaged over the specified number of time scale units.
#' 
#' @param dygraph Dygraph to add roller to
#' @param rollPeriod Number of time scale units (e.g. days, months, years) to
#'   average values over.
#'   
#' @return A dygraph that displays a range selector
#' 
#' @export
dyRoller <- function(dygraph,
                     rollPeriod = 1) {
  
  roller <- list()
  roller$showRoller <- TRUE
  roller$rollPeriod = rollPeriod
  
  # merge roller
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, roller)
  
  # return modified dygraph
  dygraph
}
