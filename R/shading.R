#' dygraph region shading
#' 
#' Specify that a region of a dygraph be drawn with a background shading
#' 
#' @param dygraph Dygraph to add shading to
#' @param from Date/time or numeric to shade from (for date/time this must be a
#'   \code{as.POSIXct} object or another object convertible via
#'   \code{as.POSIXct}).
#' @param to Date/time or numeric to shade to (for date/time this must be a
#'   \code{as.POSIXct} object or another object convertible via
#'   \code{as.POSIXct}).
#' @param color Color of shading. This can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow". Defaults to a very light gray.
#' @param axis Axis to apply shading.  Choices are "x" or "y".
#'   
#' @return A dygraph with the specified shading
#'  
#' @examples 
#' library(dygraphs)
#' 
#' dygraph(nhtemp, main = "New Haven Temperatures") %>% 
#'   dyShading(from = "1920-1-1", to = "1930-1-1") %>%
#'   dyShading(from = "1940-1-1", to = "1950-1-1")
#'   
#' dygraph(nhtemp, main = "New Haven Temperatures") %>% 
#'   dyShading(from = "48", to = "52", axis = "y") %>%
#'   dyShading(from = "50", to = "50.1", axis = "y", color = "black")
#'     
#' @note See the
#'   \href{http://rstudio.github.io/dygraphs/gallery-shaded-regions.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dyShading <- function(dygraph, from, to, color = "#EFEFEF", axis = "x") {
  
  # create shading
  shading <- list()
  shading$from <- ifelse(axis == "x" && dygraph$x$format == "date",
                         asISO8601Time(from), from)
  shading$to <- ifelse(axis == "x" && dygraph$x$format == "date",
                       asISO8601Time(to), to)
  shading$color <- color
  shading$axis <- axis
 
  # add it to list of shadings
  dygraph$x$shadings[[length(dygraph$x$shadings) + 1]] <- shading
  
  # return modified dygraph
  dygraph
}
