#' dygraph event line
#' 
#' Add a vertical event line to a dygraph
#' 
#' @param dygraph Dygraph to add event line to
#' @param date Date/time for the event (must be a \code{POSIXct} object or 
#'   another object convertible to \code{POSIXct} via \code{as.POSIXct}).
#' @param label Label for event.
#' @param labelLoc Location for label (top or bottom).
#' @param color Color of event line. This can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow". Defaults to black.
#' @param strokePattern A predefined stroke pattern type ("dotted", "dashed", or
#'   "dotdash") or a custom pattern array where the even index is a draw and odd
#'   is a space in pixels.
#'   
#' @return A dygraph with the specified event line.
#'   
#' @note See the \href{http://rstudio.github.io/dygraphs/gallery-event-lines.html}{online 
#'   documentation} for additional details and examples.
#'   
#' @examples 
#' library(dygraphs)
#' 
#' dygraph(presidents, main = "Presidential Approval") %>%
#'   dyAxis("y", valueRange = c(0, 100)) %>%
#'   dyEvent(date = "1950-6-30", "Korea", labelLoc = "bottom") %>%
#'   dyEvent(date = "1965-2-09", "Vietnam", labelLoc = "bottom")   
#'  
#' @export
dyEvent <- function(dygraph, 
                    date, 
                    label, 
                    labelLoc = c("top", "bottom"),
                    color = "black", 
                    strokePattern = "dashed") {
  
  # create event
  event <- list()
  event$date <- asISO8601Time(date)
  event$label <- label
  event$labelLoc <- match.arg(labelLoc)
  event$color <- color
  event$strokePattern <- resolveStrokePattern(strokePattern)
 
  # add it to list of events
  dygraph$x$events[[length(dygraph$x$events) + 1]] <- event
  
  # return modified dygraph
  dygraph
}
