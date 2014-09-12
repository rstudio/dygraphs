
#' Annotation for dygraph chart
#' 
#' Define a text annotation for a data-point on a dygraph chart.
#' 
#' @param series Name of data series to attach annotation to
#' @param x Date value indicating where to place the annotation. This should be 
#'   of class \code{POSIXct} or convertable to \code{POSIXct}.
#' @param text Text to overlay on the chart at the location of x
#' @param tooltip Additional tooltip text to display on mouse hover
#' @param series Series to attach the annotation to. By default, the last
#' series defined using \code{\link{dySeries}}. 
#'   
#' @return Dygraph with specified annotation
#'   
#' @export
dyAnnotation <- function(dygraph,
                         x,
                         text, 
                         tooltip = NULL,
                         series = NULL) {
  
  # convert x to date format then to a suitable time string
  x <- as.POSIXct(x, tz = "GMT")
  x <- asTimeStringGMT(x)
  
  # default the series if necessary
  if (is.null(series))
    series <- dygraph$x$attrs$labels[length(dygraph$x$attrs$labels)]
  
  # create annotation
  annotation <- list()
  annotation$x <- x
  annotation$shortText <- text
  annotation$text <- tooltip
  annotation$series <- series
  
  # add it
  dygraph$x$annotations[[length(dygraph$x$annotations) + 1]] <- annotation

  # return the dygraph
  dygraph
}
