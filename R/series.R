#' dygraph data series
#' 
#' Add a data series to a dygraph plot. Note that options will use the default 
#' global setting (as determined by \code{\link{dyOptions}}) when not specified 
#' explicitly. When no \code{dySeries} is specified for a plot then all series 
#' within the underlying data are plotted.
#' 
#' @inheritParams dyOptions
#'   
#' @param dygraph Dygraph to add a series definition to
#' @param name Name of series within data set. If no name is specified then 
#'   series are bound to implicitly based on their order within the underlying 
#'   time series object. This parameter can also be a character vector of length
#'   3 that specifies a set of input column names to use as the lower, value,
#'   and upper for a series with a shaded bar drawn around it.
#' @param label Label to display for series (uses name if no label defined)
#' @param color Color for series. These can be of the form "#AABBCC" or 
#'   "rgb(255,100,200)" or "yellow", etc. Note that if you specify a custom 
#'   color for one series then you must specify one for all series. If not 
#'   specified then the global colors option (typically based on equally-spaced 
#'   points around a color wheel). Note also that global and per-series color 
#'   specification cannot be mixed.
#' @param axis Y-axis to associate the series with ("y" or "y2")
#' @param stepPlot When set, display the graph as a step plot instead of a line 
#'   plot.
#' @param stemPlot When set, display the graph as a stem plot instead of a line
#'   plot.
#' @param fillGraph Should the area underneath the graph be filled? This option 
#'   is not compatible with error bars.
#' @param drawPoints Draw a small dot at each point, in addition to a line going
#'   through the point. This makes the individual data points easier to see, but
#'   can increase visual clutter in the chart.
#' @param pointSize The size of the dot to draw on each point in pixels. A dot 
#'   is always drawn when a point is "isolated", i.e. there is a missing point 
#'   on either side of it. This also controls the size of those dots.
#' @param pointShape The shape of the dot to draw. Can be one of the following:
#'   "dot" (default), "triangle", "square", "diamond", "pentagon", "hexagon",
#'   "circle", "star", "plus" or "ex".
#' @param strokeWidth The width of the lines connecting data points. This can be
#'   used to increase the contrast or some graphs.
#' @param strokePattern A predefined stroke pattern type ("dotted", "dashed", or
#'   "dotdash") or a custom pattern array where the even index is a draw and odd
#'   is a space in pixels. If \code{NULL} then it draws a solid line. The array 
#'   should have an even length as any odd length array could be expressed as 
#'   a smaller even length array.
#' @param strokeBorderWidth Draw a border around graph lines to make crossing 
#'   lines more easily distinguishable. Useful for graphs with many lines.
#' @param strokeBorderColor Color for the line border used if 
#'   \code{strokeBorderWidth} is set.
#' @param plotter A function which plots the data series. May also be set on on 
#'   a global basis using \code{dyOptions}. See the 
#'   \href{http://dygraphs.com/tests/plotters.html}{dygraphs documentation} for 
#'   additional details on plotting functions.
#'   
#' @return Dygraph with additional series
#'   
#' @examples
#' library(dygraphs)
#' 
#' lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
#' 
#' dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
#'   dySeries("mdeaths", drawPoints = TRUE, color = "blue") %>%
#'   dySeries("fdeaths", stepPlot = TRUE, color = "red")   
#'   
#' @note See the 
#'   \href{https://rstudio.github.io/dygraphs/gallery-series-options.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dySeries <- function(dygraph,
                     name = NULL, 
                     label = NULL,
                     color = NULL,
                     axis = "y", 
                     stepPlot = NULL,
                     stemPlot = NULL,
                     fillGraph = NULL,
                     drawPoints = NULL,
                     pointSize = NULL,
                     pointShape = NULL,
                     strokeWidth = NULL,
                     strokePattern = NULL,
                     strokeBorderWidth = NULL,
                     strokeBorderColor = NULL,
                     plotter = NULL) {
  
  # get a reference to the underlying data and labels
  data <- attr(dygraph$x, "data")
  labels <- names(data)
  
  # auto-bind name if necessary
  autobind <- attr(dygraph$x, "autoSeries")
  if (is.null(name))
    name <- labels[[autobind]]
  attr(dygraph$x, "autoSeries") <- autobind + length(name)
  
  # ensure that name is of length 1 or 3
  if (length(name) != 1 && length(name)  != 3) {
    stop("The name parameter must either be a character vector ",
         "of length one or three")
  }
  
  # Get the cols where this series is located and verify that they are
  # available within the underlying dataset
  cols <- which(labels %in% name)
  if (length(cols) != length(name)) {
    stop("One or more of the specified series were not found. ",
         "Valid series names are: ", paste(labels[-1], collapse = ", "))
  }
  
  # Data series named here are "consumed" from the automatically generated
  # list of series (they'll be added back in below)
  cols <- which(dygraph$x$attrs$labels %in% name)
  dygraph$x$data <- dygraph$x$data[-c(cols)]
  dygraph$x$attrs$labels <- dygraph$x$attrs$labels[-c(cols)]
   
  # Resolve stemPlot into a custom plotter if necessary
  plotter <- resolveStemPlot(stemPlot, plotter)
  
  # create series object
  series <- list()
  series$name <- name
  series$label <- label
  series$options <- list()
  series$options$axis <- match.arg(axis, c("y", "y2"))
  series$options$stepPlot <- stepPlot
  series$options$fillGraph <- fillGraph
  series$options$drawPoints <- drawPoints
  series$options$pointSize <- pointSize
  series$options$strokeWidth <- strokeWidth
  series$options$strokePattern <- resolveStrokePattern(strokePattern)
  series$options$strokeBorderWidth <- strokeBorderWidth
  series$options$strokeBorderColor <- strokeBorderColor
  series$options$plotter <- JS(plotter)

 
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
  
  # add color if specified 
  # This area could be supplemented to set all colors for all data series by extracting
  # the method for how dygraphs sets the underlying colors.  This would eliminate the
  # need to set all colors if only one is explicitly set
  # 
  # Also, moved this area up to grab all the colors already set before the series 
  # processed during this run through dySeries is added
  if (!is.null(color)) {
    #grab the names of all named series 
    names_ <- names(attrs$series)
   
    #grab any colors already set
    colors_ <- attrs$colors
   
    # if no colors passed thus far, set up the color vector for
    # the series defined previously
    if(is.null(colors_)) {
      colors_ <- vector('character', length(names_))
    }
    names(colors_) <- names_
    
    colors_[[name]] <- color
    
    # all options must be unnamed vectors
    names(colors_) <- NULL
    
    # attrs$colors <- as.list(c(attrs$colors, color))
    attrs$colors <- colors_
  }
  
  # default the label if we need to
  if (is.null(series$label))
    series$label <- series$name  
   
  # add label
  attrs$labels <- c(attrs$labels, series$label)
   
  # set options
  attrs$series[[series$label]] <- series$options
 
  # set attrs
  dygraph$x$attrs <- attrs

  # set point shape
  if (!is.null(pointShape)) {
    shapes <- c("dot", "triangle", "square", "diamond", "pentagon",
                "hexagon", "circle", "star", "plus", "ex")
    if (!is.element(pointShape, shapes)) {
      stop("Invalid value for pointShape parameter. ",
           "Should be one of the following: ",
           "'dot', 'triangle', 'square', 'diamond', 'pentagon', ",
           "'hexagon', 'circle', 'star', 'plus' or 'ex'")
    }

    if (pointShape != "dot") {
      dygraph$x$pointShape <- list()
      dygraph$x$pointShape[[series$label]] <- pointShape
    }
  }
  
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

# provide a custom plotter if stemPlot has been specified
resolveStemPlot <- function(stemPlot, plotter) {
  
  # check for stemPlot argument
  if (isTRUE(stemPlot)) {
    
    # verify that a custom plotter hasn't been specified
    if (!is.null(plotter)) {
      stop("stemPlot provides it's own plotter so is incompatible with ",
           "specifying a custom plotter", call. = FALSE)
    }
    
    # provide custom plotter JS
    "function stemPlotter(e) { 
       var ctx = e.drawingContext; 
       var points = e.points; 
       var y_bottom = e.dygraph.toDomYCoord(0);
       ctx.fillStyle = e.color; 
       for (var i = 0; i < points.length; i++) { 
          var p = points[i]; 
          var center_x = p.canvasx;
          var center_y = p.canvasy; 
          ctx.beginPath(); 
          ctx.moveTo(center_x, y_bottom); 
          ctx.lineTo(center_x, center_y); 
          ctx.stroke();
          ctx.beginPath(); 
          ctx.arc(center_x, center_y, 3, 0, 2*Math.PI); 
          ctx.stroke();
       }
    }"
  } else {
    # specified plotter
    plotter
  }
}








