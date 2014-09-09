
#' dygraph data series
#' 
#' Add a data series to a dygraph plot. Note that options will use the 
#' default global setting (as determined by \code{\link{dyOptions}}) when not 
#' specified explicitly. When no \code{dySeries} is specified for a 
#' plot then all series within the underlying data are plotted. If a single
#' call to \code{dySeries} is made however then only those series added
#' explicitly via \code{dySeries} are plotted.
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
#' @return Dygraph with additional series
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
  
  # get a reference to the underlying data and labels
  data <- attr(dygraph$x, "data")
  labels <- names(data)
  
  # if this is the first custom series then reset our data fields
  # to just include the first column (x values). 
  if (!attr(dygraph$x, "customSeries")) {
    attr(dygraph$x, "customSeries") <- TRUE
    dygraph$x$data <- list(data[[1]])
    dygraph$x$attrs$labels <- labels[[1]]
  }
  
  # create series object
  series <- list()
  series$name <- name
  series$label <- label
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
  
  # resolve multi-series
  if (length(series$name) == 3) {
    
    # find column indexes within the data
    cols <- integer(3)
    for (i in 1:3) {
      col <- which(labels == series$name[[i]])
      if (length(col) != 1)
        stop("Series name '", series$name[[i]], "' not found in input data")
      cols[[i]] <- col
    }
    
    # mark attrs as containing custom bars
    attrs$customBars <- TRUE
    
    # fixup name
    series$name <- series$name[[2]]
    
    # compute series data
    seriesData <- toMultiSeries(data[[cols[[1]]]],
                                data[[cols[[2]]]],
                                data[[cols[[3]]]])
    
  } else {
    # select series data
    seriesData <- data[[series$name]]
  }
  
  # default the label if we need to
  if (is.null(series$label))
    series$label <- series$name  
   
  # add label
  attrs$labels <- c(attrs$labels, series$label)
   
  # set options
  attrs$series[[series$label]] <- series$options
  
  # set color if specified 
  if (!is.null(color)) {
    if (is.null(attrs$colors))
      attrs$colors <- c()
    attrs$colors[[length(attrs$labels) - 1]] <- color
    attrs$colors[is.na(attrs$colors)] <- "black" # default missing
  }
  
  # set attrs
  dygraph$x$attrs <- attrs
  
  # add data
  dygraph$x$data[[length(dygraph$x$data) + 1]] <- seriesData
  
  # if we are using custom bars then fixed non custom-bar data
  # series to still display
  if (isTRUE(attrs$customBars)) {
    for (i in 2:length(dygraph$x$data)) {
      if (length(dygraph$x$data[[i]][[1]]) == 1) {
        values <- dygraph$x$data[[i]]
        dygraph$x$data[[i]] <- toMultiSeries(values, values, values)
      }
    }
  }
  
  # return modified dygraph
  dygraph
}


#' Add series data to dygraph
#' 
#' Add an additional column of series data to a dygraph. This
#' is typically used in the construction of custom series types
#' (e.g. log scaled, smoothed, etc.)
#' 
#' @param dygraph Dygraph to add data to
#' @param name Name of series
#' @param values Data values
#' 
#' @return Dygraph with additional series data 
#' 
#' @export 
dySeriesData <- function(dygraph, name, values) {
  
  # add values
  data <- attr(dygraph$x, "data")
  data[[name]] <- values
  attr(dygraph$x, "data") <- data
  
  # return modified dygraph
  dygraph
}


# return a list of three element arrays 
toMultiSeries <- function(lower, value, upper) {  
  series <- vector(mode = "list", length = length(value))
  for (i in 1:length(series))
    series[[i]] <- c(lower[[i]], value[[i]], upper[[i]])
  series
}












