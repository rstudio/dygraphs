#' dygraph interactive range selection and zooming
#'
#' Add a range selector to the bottom of the chart that allows users to pan and
#' zoom to various date ranges.
#'
#' @inheritParams dyOptions
#'
#' @param dygraph Dygraph to add range selector to
#' @param dateWindow Initially zoom in on a section of the graph. Is a two
#'   element vector [earliest, latest], where earliest/latest objects
#'   convertible via \code{as.POSIXct}.
#' @param height Height, in pixels, of the range selector widget.
#' @param fillColor The range selector mini plot fill color. This can be of the
#'   form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify ""
#'   to turn off fill.
#' @param strokeColor The range selector mini plot stroke color. This can be of
#'   the form "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify
#'   "" to turn off stroke.
#' @param keepMouseZoom Keep mouse zoom when adding a range selector
#'
#' @return A dygraph that displays a range selector
#'
#' @examples
#' library(dygraphs)
#'
#' dygraph(nhtemp, main = "New Haven Temperatures") %>%
#'   dyRangeSelector()
#'
#' dygraph(nhtemp, main = "New Haven Temperatures") %>%
#'   dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))
#'
#' dygraph(nhtemp, main = "New Haven Temperatures") %>%
#'   dyRangeSelector(height = 20, strokeColor = "")
#' @note See the
#' \href{https://rstudio.github.io/dygraphs/gallery-range-selector.html}{online
#' documentation} for additional details and examples.
#'
#' Shiny applications can respond to changes in the \code{dateWindow}
#' via a special date window shiny input value. For example, if the
#' output id of a dygraph is `series` then the current date window
#' can be read from \code{input$series_date_window} as an array of
#' two date values (from and to).
#'
#' @export
dyRangeSelector <- function(dygraph,
                            dateWindow = NULL,
                            height = 40,
                            fillColor = " #A7B1C4",
                            strokeColor = "#808FAB",
                            keepMouseZoom = TRUE,
                            retainDateWindow = FALSE) {
  selector <- list()
  selector$showRangeSelector <- TRUE
  if (!is.null(dateWindow)) {
    if (length(dateWindow) != 2) {
      stop("dateWindow must be vector of length 2 that is convertible to POSIXct")
    }
    if (dygraph$x$format == "date") {
      selector$dateWindow <- sapply(USE.NAMES = FALSE, dateWindow, asISO8601Time)
    } else {
      selector$dateWindow <- dateWindow
    }
  }
  selector$rangeSelectorHeight <- height
  selector$rangeSelectorPlotFillColor <- fillColor
  selector$rangeSelectorPlotStrokeColor <- strokeColor
  if (!missing(retainDateWindow)) {
    selector$retainDateWindow <- retainDateWindow
  }

  if (keepMouseZoom) {
    selector$interactionModel <- JS("Dygraph.Interaction.defaultModel")
  }

  # merge selector
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, selector)

  # return modified dygraph
  dygraph
}
