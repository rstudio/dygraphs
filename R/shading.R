#' dygraph region shading
#' 
#' Specify that a region of a dygraph be drawn with a background shading
#' 
#' @param dygraph Dygraph to add shading to
#' @param from Date/time to shade from (must be a \code{as.POSIXct} object or 
#'   another object convertible to \code{as.POSIXct}). convertable via 
#'   \code{as.POSIXct}).
#' @param to Date/time to shade to (must be a \code{as.POSIXct} object or 
#'   another object convertible to \code{as.POSIXct}). convertable via 
#'   \code{as.POSIXct}).
#' @param color Color of shading. This can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow". Defaults to a very light gray.
#'   
#' @return A dygraph with the specified shading
#'   
#' @note See the
#'   \href{http://rstudio.github.io/dygraphs/gallery-shaded-regions.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dyShading <- function(dygraph, from, to, color = "#EFEFEF") {
  
  # create shading
  shading <- list()
  shading$from <- asISO8601Time(from)
  shading$to <- asISO8601Time(to)
  shading$color <- color
 
  # add it to list of shadings
  dygraph$x$shadings[[length(dygraph$x$shadings) + 1]] <- shading
  
  # return modified dygraph
  dygraph
}
