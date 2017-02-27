#' dygraph trade arrow
#'
#' Add an arrow to a dygraph
#'
#' @param dygraph Dygraph to add arrow to
#' @param seriesName Name of series within data set.
#' @param x Either numeric or date/time for the event, depending on the format of the
#'   x-axis of the dygraph. (For date/time must be a \code{POSIXct} object or another
#'   object convertible to \code{POSIXct} via \code{as.POSIXct})
#' @param label Label for event. Defaults to blank
#' @param direction Direction of arrow (up or down)
#' @param fillColor Fill color of arrow. This can be of the form "#AABBCC" or
#'   "rgb(255,100,200)" or "yellow". Defaults to white.
#' @param strokeColor Stroke color of arrow. This can be of the form "#AABBCC" or
#'   "rgb(255,100,200)" or "yellow". Defaults to black.
#'
#' @return A dygraph with the specified trade arrow.
#'
#' @examples
#' library(dygraphs)
#' dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
#'   dyArrow("1950-6-30", "Korea", direction = "down", fillColor = "red") %>%
#'   dyArrow("1965-2-09", "Vietnam", direction = "down", fillColor = "red")
#'
#' dygraph(presidents, main = "Quarterly Presidential Approval Ratings") %>%
#'   dyArrow(c("1950-6-30", "1965-2-09"),
#'           c("Korea", "Vietnam"),
#'           direction = "down",
#'           fillColor = "red")
#'
#' @export
dyArrow <- function(dygraph,
                    x,
                    text = NULL,
                    tooltip = NULL,
                    direction = c("up", "down", "left", "right", "ne", "se", "sw", "nw"),
                    fillColor = "white",
                    strokeColor = "black",
                    series = NULL) {

  # create arrows
  if (!is.null(text) && length(x) != length(text)) {
    stop("Length of 'x' and 'text' does not match")
  }
  if (!is.null(tooltip) && length(x) != length(tooltip)) {
    stop("Length of 'x' and 'tooltip' does not match")
  }

  # validate series if specified
  if (!is.null(series) && ! series %in% dygraph$x$attrs$labels) {
    stop("The specified series was not found. Valid series names are: ",
         paste(dygraph$x$attrs$labels[-1], collapse = ", "))
  }

  # default the series if necessary
  if (is.null(series))
    series <- dygraph$x$attrs$labels[length(dygraph$x$attrs$labels)]

  fixedtz <- ifelse(is.null(dygraph$x$fixedtz), FALSE, dygraph$x$fixedtz)
  scale <- dygraph$x$scale

  direction <- match.arg(direction)
  arrows <- lapply(seq_along(x),
                   function(i) {
                     if (is.null(text))
                       arrowText <- NULL
                     else
                       arrowText <- text[i]
                     if (is.null(tooltip))
                       arrowTooltip <- NULL
                     else
                       arrowTooltip <- tooltip[i]
                     list(xval = ifelse(dygraph$x$format == "date",
                                        asISO8601Time(x[i]), x[i]),
                          text = arrowText,
                          tooltip = arrowTooltip,
                          direction = direction,
                          fillColor = fillColor,
                          strokeColor = strokeColor,
                          series = series,
                          fixedtz = fixedtz,
                          scale = scale)
                   })

  pluginName <- "Arrow"
  if (dyHasPlugin(dygraph, pluginName)) {
    dygraph <- dySetPluginOptions(dygraph,
                                  pluginName,
                                  append(dyGetPluginOptions(dygraph, pluginName),
                                         arrows))
  } else {
    dygraph <- dyPlugin(dygraph = dygraph,
                        name = pluginName,
                        path = system.file("plugins/arrow.js",
                                           package = "dygraphs"),
                        options = arrows)
  }

  # return dygraph
  dygraph
}
