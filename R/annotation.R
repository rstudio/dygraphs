
#' Annotation for dygraph chart
#'
#' Define a text annotation for a data-point on a dygraph chart.
#'
#' @param dygraph Dygraph to add an annotation to
#' @param x Either numeric or date value indicating where to place the
#'   annotation. For date value, this should be of class \code{POSIXct} or
#'   convertible to \code{POSIXct}.
#' @param text Text to overlay on the chart at the location of x
#' @param tooltip Additional tooltip text to display on mouse hover
#' @param width Width (in pixels) of the annotation flag.
#' @param height Height (in pixels) of the annotation flag.
#' @param cssClass CSS class to use for styling the annotation.
#' @param tickHeight Height of the tick mark (in pixels) connecting the point to
#'   its flag or icon.
#' @param attachAtBottom If true, attach annotations to the x-axis, rather than
#'   to actual points.
#' @param clickHandler JavaScript function to call when an annotation is
#'   clicked.
#' @param mouseOverHandler JavaScript function to call when the mouse hovers
#'   over an annotation.
#' @param mouseOutHandler JavaScript function to call when the mouse exits an
#'   annotation.
#' @param dblClickHandler JavaScript function to call when an annotation is
#'   double clicked.
#' @param series Series to attach the annotation to. By default, the last series
#'   defined using \code{\link{dySeries}}.
#'
#' @note Annotations are bound to specific series in the input data. If you have
#'   only one series or if you define annotations immediately after a call to
#'   \code{\link{dySeries}} then you need not specify the series explicitly.
#'   Otherwise, you should use the \code{series} parameter to indicate which
#'   series the annotation should be bound to.
#'
#'   Annotation event handlers can also specified globally (see
#'   \code{\link{dyCallbacks}}).
#'
#' @note See the
#'   \href{https://rstudio.github.io/dygraphs/gallery-annotations.html}{online
#'   documentation} for additional details and examples.
#'
#' @return Dygraph with specified annotation
#'
#' @examples
#' library(dygraphs)
#'
#' dygraph(presidents, main = "Presidential Approval") %>%
#'   dyAxis("y", valueRange = c(0, 100)) %>%
#'   dyAnnotation("1950-7-1", text = "A", tooltip = "Korea") %>%
#'   dyAnnotation("1965-1-1", text = "B", tooltip = "Vietnam")
#' @export
dyAnnotation <- function(dygraph,
                         x,
                         text,
                         tooltip = NULL,
                         width = NULL,
                         height = NULL,
                         cssClass = NULL,
                         tickHeight = NULL,
                         attachAtBottom = FALSE,
                         clickHandler = NULL,
                         mouseOverHandler = NULL,
                         mouseOutHandler = NULL,
                         dblClickHandler = NULL,
                         series = NULL) {

  # convert x to date format then to a suitable time value if it is date format
  if (dygraph$x$format == "date") {
    x <- asISO8601Time(x)
  }

  # validate series if specified
  if (!is.null(series) && !series %in% dygraph$x$attrs$labels) {
    stop(
      "The specified series was not found. Valid series names are: ",
      paste(dygraph$x$attrs$labels[-1], collapse = ", ")
    )
  }

  # default the series if necessary
  if (is.null(series)) {
    series <- dygraph$x$attrs$labels[length(dygraph$x$attrs$labels)]
  }

  # create annotation
  annotation <- list()
  annotation$x <- x
  annotation$shortText <- text
  annotation$text <- tooltip
  annotation$width <- width
  annotation$height <- height
  annotation$cssClass <- cssClass
  annotation$tickHeight <- tickHeight
  annotation$attachAtBottom <- attachAtBottom
  annotation$clickHandler <- JS(clickHandler)
  annotation$mouseOverHandler <- JS(mouseOverHandler)
  annotation$mouseOutHandler <- JS(mouseOutHandler)
  annotation$dblClickHandler <- JS(dblClickHandler)
  annotation$series <- series

  # add it
  dygraph$x$annotations[[length(dygraph$x$annotations) + 1]] <- annotation

  # return the dygraph
  dygraph
}
