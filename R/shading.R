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
#'   \href{https://rstudio.github.io/dygraphs/gallery-annotations.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dyShading <- function(dygraph, from, to, color = "#EFEFEF", axis = "x") {
  l_from <- length(from)
  l_to <- length(to)
  l_color <- length(color)
  n <- min(l_from, l_to)
  # parameters check
  if (l_from != l_to) warning(paste("'from' and 'to' vectors don't have the same length:",
                                    l_from , "and", l_to,
                                    ". Only keeping the", n, "first value(s)"))
  if (l_color == 1) color <- rep(color, n)
  if (l_color < n) color <- c(color, rep("#EFEFEF", n - l_color))
  if (l_color > n) color <- head(color, n)
  
  if (axis == "x" && dygraph$x$format == "date") {
    from <- asISO8601Time(from)
    to <- asISO8601Time(to)
  }

  # create shading list
  shading <- vector(mode = "list", length = n)
  for (i in seq_along(shading)) {
    shading[[i]] <- list(from = from[i],
                         to = to[i],
                         color = color[i],
                         axis = axis)
  }
 
  # add it to list of shadings
  dygraph$x$shadings <- c(dygraph$x$shadings, shading)
  
  # return modified dygraph
  dygraph
}
