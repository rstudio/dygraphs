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
#'   "rgb(255,100,200)" or "yellow", etc. If not specified then the global 
#'   colors option (typically based on equally-spaced points around a color wheel). 
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
#'   \href{https://dygraphs.com/tests/plotters.html}{dygraphs documentation} for 
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
  
  # when setting up the first color, we start handling colors here 
  if (!is.null(color) && is.null(dygraph$x$attrs$colors)) {
      colors <- dygraphColors(dygraph, length(labels) - 1)
      dygraph$x$attrs$colors <- colors
  }

  # prepare the colors list for processing 
  if (!is.null(dygraph$x$attrs$colors)) {
     colors <- dygraph$x$attrs$colors
     names(colors) <- dygraph$x$attrs$labels[-1]
  }
   
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
  
  # grab the colors for the series being processed
  if (!is.null(dygraph$x$attrs$colors)) {
    currColors <- colors[names(colors) %in% name]

    if (!is.null(color)) 
			currColors[[series$name]] <- color
    
    colors <- colors[!names(colors) %in% name]
    colors[[series$name]] <- currColors[[series$name]]
    
    # compensating for the bug whereas a single series dygraph with specified color
    # shows up as black since the DateTime series tries to take the first color
    if (length(colors) == 1) colors <- c(colors, colors)
    
    attrs$colors <- colors
    names(attrs$colors) <- NULL
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
      if (is.null(dygraph$x$pointShape)) {
        dygraph$x$pointShape <- list()
      }
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

dygraphColors <- function(dygraph, num) {

  # These are used for when no custom colors are specified.
  sat <- dygraph$x$attrs$colorSaturation %||% 1.0
  val <- dygraph$x$attrs$colorValue %||% 0.5
  half <- ceiling(num / 2)
  
  colors<-c()
  
  for (i in 0:(num-1)) {
    # alternate colors for high contrast.
    idx <- ifelse(i %% 2, (half + (i + 1) / 2), ceiling((i + 1) / 2))
    hue <- (1.0 * idx / (1 + num))
    color <- hsvToRGB(hue, sat, val)
    colors <- c(colors, color)

  }

  return(colors)
}

#' @importFrom grDevices rgb
hsvToRGB <- function (hue, saturation, value) {
  if (saturation == 0) {
    red = value
    green = value
    blue = value

  } else {
    i <- floor(hue * 6)
    f <- (hue * 6) - i
    p <- value * (1 - saturation)
    q <- value * (1 - (saturation * f))
    t <- value * (1 - (saturation * (1 - f)))

    # converting the switch from the JS library to a vector selection    
    red <- c(value, q, p, p, t, value, value)
    green <- c(t, value, value, q, p, p, t)
    blue <- c(p, p, t, value, value, q, p)
    
    r <- red[i + 1]
    g <- green[i + 1]
    b <- blue[i + 1]
  }

  red <- floor(255 * r + 0.5)
  green <- floor(255 * g + 0.5)
  blue <- floor(255 * b + 0.5)
  return (rgb(red, green, blue, maxColorValue = 255))
}

`%||%` <- function(a, b){
  if(!is.null(a)) return(a)
  return(b)
}

