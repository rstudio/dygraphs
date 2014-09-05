
#' dygraph data series options
#' 
#' Add per-series options to a dygraph plot. Note that options will use the
#' default global setting (as determined by \code{\link{dyOptions}}) when not
#' specified explicitly.
#' 
#' @inheritParams dyOptions
#'   
#' @param name Name of series within dataset (unamed series can be bound by 
#'   order or using the convention V1, V2, etc.). This can also be a character 
#'   vector of length 3 that specifies a set of input series to use as the 
#'   lower, value, and upper values for a series with a shared bar drawn around 
#'   it. In this case the \code{label} parameter must also be specified to 
#'   provide a label for the aggregate series.
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
dySeries <- function(name = NULL, 
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
  
  # ensure that name is either NULL or of length 1 or 3
  if (!is.null(name) && length(name) != 1 && length(name)  != 3) {
    stop("The name parameter must either be NULL, a single ",
         "character value, or a character value of length 3")
  }
  
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
  structure(series, class = "dygraph.series")
}


addSeries <- function (attrs, series) {
    
  if (length(series) > 0) {
    colors = character(length(series))
    for (i in 1:length(series)) { 
      
      # copy the series and validate it
      s <- series[[i]]
      if (!inherits(s, "dygraph.series"))
        stop("You must pass only dySeries objects in the series parameter")
      
      # record color
      if (!is.null(s$color))
        colors[[i]] <- s$color
      
      # if this is a named series then find it's index
      # and re-bind i to it
      if (!is.null(s$name)) {
        m <- match(s$name, attrs$labels)
        if (!is.na(m))
          i <- m - 1
      }
      
      # custom label if requested
      if (!is.null(s$label))
        attrs$labels[[i + 1]] <- s$label
      
      # set series options
      name <- attrs$labels[[i + 1]]
      attrs$series[[name]] <- s$options
    }
    
    # resolve colors (if one specified then all must be specified)
    colors <- colors[colors != ""]
    if (length(colors) > 0) {
      if (length(colors) == length(series)) {
        attrs$colors <- colors
      } else {
        stop("If you specify one custom series color you must specify ",
             "a color for all series")
      }
    }
  }
  attrs
}


haveCustomBars <- function(series) {
  if (!is.null(series) && length(series) > 0) {
    for (i in 1:length(series))
      if (length(series[[i]]$name) == 3)
        return(TRUE)
  }
  FALSE
}


resolveCustomBars <- function(data, series) {
  
  seriesNames <- character()
  for (i in 1:length(series)) { 
    
    s <- series[[i]]
    
    if (length(s$name) == 3) {
      
      # get the names
      names <- s$name
      
      # compute the multi series
      multiSeries <- toMultiSeries(data[[names[[1]]]], 
                                   data[[names[[2]]]],
                                   data[[names[[3]]]])
      
      # remove those columns from the named list
      data[names(data) %in% names] <- NULL
          
      # set multi-series (using the value column)
      s$name <- names[[2]]
      data[[s$name]] <- multiSeries
      
      # track series names
      seriesNames <- c(seriesNames, s$name)
    }
    
    series[[i]] <- s
  }
  
  # for dataset elements not named in a multi-series, provide
  # three values so that they can still be displayed
  columns <- names(data)
  columns <- columns[!columns %in% seriesNames]
  for (column in columns) {
    values <- data[[column]]
    data[[column]] <- toMultiSeries(values, values, values)
  }
  
  # return resolved dataset and series
  list(data = data, series = series)
}

# return a list of three element arrays 
toMultiSeries <- function(lower, value, upper) {  
  series <- vector(mode = "list", length = length(value))
  for (i in 1:length(series))
    series[[i]] <- c(lower[[i]], value[[i]], upper[[i]])
  series
}












