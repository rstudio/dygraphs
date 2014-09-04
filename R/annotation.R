
#' Annotation for dygraph chart
#' 
#' Define a text annotation for a data-point on a dygraph chart.
#' 
#' @param x Date value indicating where to place the annotation. This should be 
#'   of class \code{POSIXct} or convertable to \code{POSIXct}.
#' @param shortText Text to overlay on the chart at the location of x
#' @param text Additional tooltip text to display on mouse hover
#' @param series Name of data series to attach annotation to (not required if 
#'   there is only one series in the chart).
#'   
#' @return Annotation definition
#'   
#' @export
dyAnnotation <- function(x, shortText, text = NULL, series = NULL) {
  
  # convert x to date format then to milliseconds since epoch
  x <- as.POSIXct(x, tz = "GMT")
  x <- as.double(x) * 1000
  
  # create annotation
  annotation <- list()
  annotation$series <- series
  annotation$x <- x
  annotation$shortText <- shortText
  annotation$text <- text
  structure(annotation, class = "dygraph.annotation")
}

getAnnotations <- function(annotations, colNames) {
  
  # exclude if annotations is null or empty
  if (is.null(annotations) || length(annotations) == 0)
    return(NULL)
  
  # is there a default series?
  if (length(colNames) == 1)
    defaultSeries <- colNames[[1]]
  else
    defaultSeries <- NULL
  
  # see if we need to provide a defaultSeries for any of the annotations
  for (i in 1:length(annotations)) {
    annotation <- annotations[[i]]
    if (is.null(annotation$series)) {
      if (!is.null(defaultSeries)) {  
        annotation$series <- defaultSeries
        annotations[[i]] <- annotation
      } else {
        stop("There is more than one series on this graph so you must ",
             "provide an explicit series name for your annotations")
      }
    }
  }
  annotations
}