
#' dygraph data series options
#' 
#' Add per-series options to a dygraph plot. Note that options will use the 
#' default global setting (as determined by \code{\link{dyOptions}}) when not 
#' specified explicitly.
#' 
#' @inheritParams dyOptions
#'   
#' @param dygraph Dygraph to add a series definition to
#' @param name Name of series within dataset (unamed series can be bound by 
#'   using the convention V1, V2, etc.). This can also be a character vector of
#'   length 3 that specifies a set of input series to use as the lower, value,
#'   and upper for a series with a shaded bar drawn around it.
#' @param label Label to display for series (uses name if no label defined)
#' @param color Color for series. These can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow", etc. Note that if you specify a custom 
#'   color for one series then you must specify one for all series. If not 
#'   specified, equally-spaced points around a color wheel are used.
#' @param axis Y-axis to associate the series with ("y" or "y2")
#' @param stepPlot When set, display the graph as a step plot instead of a line 
#'   plot.
#' @param fillGraph Should the area underneath the graph be filled? This option 
#'   is not compatible with error bars.
#' @param drawPoints Draw a small dot at each point, in addition to a line going
#'   through the point. This makes the individual data points easier to see, but
#'   can increase visual clutter in the chart.
#' @param pointSize The size of the dot to draw on each point in pixels. A dot 
#'   is always drawn when a point is "isolated", i.e. there is a missing point 
#'   on either side of it. This also controls the size of those dots.
#' @param strokeWidth The width of the lines connecting data points. This can be
#'   used to increase the contrast or some graphs.
#' @param strokePattern A custom pattern array where the even index is a draw 
#'   and odd is a space in pixels. If null then it draws a solid line. The array
#'   should have a even length as any odd lengthed array could be expressed as a
#'   smaller even length array. This is used to create dashed lines.
#' @param strokeBorderWidth Draw a border around graph lines to make crossing 
#'   lines more easily distinguishable. Useful for graphs with many lines.
#' @param strokeBorderColor Color for the line border used if 
#'   \code{strokeBorderWidth} is set.
#'   
#' @return Series options
#'   
#' @export
dySeries <- function(dygraph,
                     name, 
                     label = NULL,
                     color = NULL,
                     axis = "y", 
                     stepPlot = NULL,
                     fillGraph = NULL,
                     drawPoints = NULL,
                     pointSize = NULL,
                     strokeWidth = NULL,
                     strokePattern = NULL,
                     strokeBorderWidth = NULL,
                     strokeBorderColor = NULL,
                     ...) {
  
  # ensure that name is of length 1 or 3
  if (length(name) != 1 && length(name)  != 3) {
    stop("The name parameter must either be a character vector ",
         "of length one or three")
  }
  
  # create series object
  series <- list()
  series$name <- name
  series$label <- label
  series$color <- color
  series$options <- list(...)
  series$options$axis <- match.arg(axis, c("y", "y2"))
  series$options$stepPlot <- stepPlot
  series$options$fillGraph <- fillGraph
  series$options$drawPoints <- drawPoints
  series$options$pointSize <- pointSize
  series$options$strokeWidth <- strokeWidth
  series$options$strokePattern <- strokePattern
  series$options$strokeBorderWidth <- strokeBorderWidth
  series$options$strokeBorderColor <- strokeBorderColor
 
  # copy attrs for modification
  attrs <- dygraph$x$attrs
  
  # if there is a custom color and no colors field
  # exists then allocate it
  if (!is.null(series$color) && is.null(attrs$colors))
    attrs$colors <- rep("black", length(attrs$labels) - 1)
  
  # resolve multi-series
  if (length(series$name) == 3) {
    
    # find column indexes within the data
    cols <- integer(3)
    for (i in 1:3) {
      col <- which(attrs$labels == series$name[[i]])
      if (length(col) != 1)
        stop("Series name '", series$name[[i]], "' not found in input data")
      cols[[i]] <- col
    }
    
    # mark attrs as containing custom bars
    attrs$customBars <- TRUE
    
    # compute multi-series
    multiData <- toMultiSeries(dygraph$x$data[[cols[[1]]]],
                               dygraph$x$data[[cols[[2]]]],
                               dygraph$x$data[[cols[[3]]]])
    
    # remove the upper and lower slots
    attrs$labels <- attrs$labels[-c(cols[[1]], cols[[3]])]
    dygraph$x$data <- dygraph$x$data[-c(cols[[1]], cols[[3]])]
    
    # fixup label and name
    series$name <- series$name[[2]]
    if (is.null(series$label))
      series$label <- series$name  
    
    # set the new series data
    col <- which(attrs$labels == series$name)
    dygraph$x$data[[col]] <- multiData
  }
  
  # determine column
  col <- which(attrs$labels == series$name)
  if (length(col) != 1)
    stop("Series name '", series$name, "' not found in input data")
  
  # color
  if (!is.null(series$color))
    attrs$colors[[col - 1]] <- series$color
  
  # default the label if we need to
  if (is.null(series$label))
    series$label <- series$name
  
  # set into labels
  attrs$labels[[col]] <- series$label
  
  # options
  attrs$series[[series$label]] <- series$options
  
  # return modified dygraph
  dygraph$x$attrs <- attrs
  dygraph
}

# return a list of three element arrays 
toMultiSeries <- function(lower, value, upper) {  
  series <- vector(mode = "list", length = length(value))
  for (i in 1:length(series))
    series[[i]] <- c(lower[[i]], value[[i]], upper[[i]])
  series
}












