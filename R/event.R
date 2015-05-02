#' dygraph event line
#' 
#' Add a vertical event line to a dygraph
#' 
#' @param dygraph Dygraph to add event line to
#' @param pos If axis is "x", Date/time for the event 
#'   (must be a \code{POSIXct} object or 
#'   another object convertible to \code{POSIXct} via \code{as.POSIXct}); if
#'   axis is "y", numeric location of event.
#' @param label Label for event.
#' @param labelLoc Location for label.  If axis is "x", top or bottom; if 
#'   axis is "y", left or right.
#' @param color Color of event line. This can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow". Defaults to black.
#' @param strokePattern A predefined stroke pattern type ("dotted", "dashed",
#'   "dotdash", or "solid") or a custom pattern array where the even index is a draw and odd
#'   is a space in pixels.
#' @param axis Axis to add event.  Choices are "x" or "y".
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
#'   dyEvent("1950-6-30", "Korea", labelLoc = "bottom") %>%
#'   dyEvent("1965-2-09", "Vietnam", labelLoc = "bottom") %>% 
#'   dyEvent(max(presidents, na.rm = TRUE), "Max", axis = "y",
#'           strokePattern = "solid", color = "blue")
#'  
#' @export
dyEvent <- function(dygraph, 
                    pos,
                    label, 
                    labelLoc = c("top", "bottom", "left", "right"),
                    color = "black", 
                    strokePattern = "dashed", 
                    axis = "x") {
  
  # create event
  event <- list()
  event$pos = ifelse(axis == "x", asISO8601Time(pos), pos)
  if (!missing(label) && !is.null(label)) event$label <- label
  event$labelLoc <- match.arg(labelLoc)
  event$color <- color
  event$strokePattern <- resolveStrokePattern(strokePattern)
  event$axis <- axis
 
  # add it to list of events
  dygraph$x$events[[length(dygraph$x$events) + 1]] <- event
  
  # return modified dygraph
  dygraph
}
