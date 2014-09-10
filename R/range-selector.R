#' dygraph interactive range selection and zooming
#' 
#' Add a range selector to the bottom of the chart that allows users to pan and 
#' zoom to various date ranges.
#' 
#' @param dygraph Dygraph to add range selector to
#' @param dateWindow Initially zoom in on a section of the graph. Is a two 
#'   element vector [earliest, latest], where earliest/latest objects 
#'   convertable via \code{as.POSIXct}.
#' @param height Height, in pixels, of the range selector widget. This option 
#'   can only be specified at Dygraph creation time.
#' @param fillColor The range selector mini plot fill color. This can be of the 
#'   form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify 
#'   "" to turn off fill.
#' @param strokeColor The range selector mini plot stroke color. This can be of
#'   the form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify
#'   "" to turn off stroke.
#'   
#' @return A dygraph that displays a range selector
#'   
#' @note
#' See the \href{http://jjallaire.github.io/dygraphs/}{online documentation} for
#' additional details and examples.  
#' 
#' @export
dyRangeSelector <- function(dygraph,
                            dateWindow = NULL, 
                            height = 40,
                            fillColor = " #A7B1C4",
                            strokeColor = "#808FAB") {
  
  selector <- list()
  selector$showRangeSelector = TRUE
  if (!is.null(dateWindow)) {
    if (length(dateWindow) != 2)
      stop("dateWindow must be vector of length 2 that is convertible to POSIXct")
    selector$dateWindow <- sapply(USE.NAMES = FALSE, dateWindow, 
                                  function(x) { 
                                    as.double(as.POSIXct(x, tz = "GMT")) * 1000; 
                                  }
    );
  }
  selector$rangeSelectorHeight <- height
  selector$rangeSelectorPlotFillColor <- fillColor
  selector$rangeSelectorPlotStrokeColor <- strokeColor
  
  # merge selector
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, selector)
  
  # return modified dygraph
  dygraph
}

