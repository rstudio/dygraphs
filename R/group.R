#' dygraph series group
#' 
#' Add a data series group to a dygraph plot. Note that options will use the default 
#' global setting (as determined by \code{\link{dyOptions}}) when not specified 
#' explicitly. Importantly, any dySeries options passed can be passed as a vector of values 
#' and will be replicated across all series named as part of the group. If arguments differ in
#' length than the number of series named, then the argument vector will be 
#' cycled across the named series.
#' 
#' NOTE: dyGroup will turn off \code{stackedGraph}, as the option will calculated cumulatives using
#' all series in the underlying dygraph, not just a subset.
#' 
#' The dyGroup function can also replicated similar arguments across multiple series, or 
#' be used to apply a grouped custom plotter - i.e., multi-column plotter - to a subset of the
#' dygraph data series.
#' 
#' @inheritParams dySeries
#' @inheritParams dyOptions
#'   
#' @param dygraph Dygraph to add a series definition to
#' @param name character vector of the series within data set. If no name is specified then 
#'   series are bound to implicitly based on their order within the underlying 
#'   time series object. This parameter can also be a character vector of length
#'   3 that specifies a set of input column names to use as the lower, value,
#'   and upper for a series with a shaded bar drawn around it.
#' @param label Labels to display for series (uses name if no label defined)
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
#' 
#' @param plotter A function which plots the series group. See the 
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
#'   dySeries("fdeaths", stepPlot = TRUE, color = "red") %>% 
#'   dyGroup(c("mdeaths", "ldeaths"), drawPoints = TRUE, color = c("blue", "green"))
#'
#' @note See the 
#'   \href{https://rstudio.github.io/dygraphs/gallery-series-options.html}{online
#'   documentation} for additional details and examples.
#'   
#' @export
dyGroup <- function(dygraph,
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

  if (length(plotter) > 1) message('dyGroup: pass only a single plotter option')
  
  
  # auto-bind name if necessary
  autobind <- attr(dygraph$x, "autoSeries")
  if (length(name) == 1) {
    dygraph<-dySeries(dygraph = dygraph, 
                      name = name, 
                      label = label, 
                      color = color, 
                      plotter = plotter)
    return(dygraph)
  }
  
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
    
  # Plotter-mod!  Added the plotter != NULL test to keep base capability while
  # expanding to include group plotters
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

   
  # MUST turn off native stacking option, as underlying dygraph 
  # will include custom-plotted points in the stacked calculation
  if (length(dygraph$x$attrs$stackedGraph) > 0) {
    if (dygraph$x$attrs$stackedGraph) 
						warning("dyGroup is incompatible with stackedGraph... stackedGraph now FALSE")

    dygraph$x$attrs$stackedGraph <- FALSE;
  }
   
  # Resolve stemPlot into a custom plotter if necessary
  plotter <- resolveStemPlot(stemPlot, plotter)
  
  if (!is.null(pointShape))
    dygraph$x$pointShape <- list()
   
  l <- length(name)
  
  # copy attrs for modification
  attrs <- dygraph$x$attrs
  
  # repeat (most of) the steps from dySeries, just in a loop 
  for (i in 1:l) {
    
    # create series object
    series <- list()
    series$name <- name[i]
    
    # take the passed options and extend to the length of the name vector; it's
    # up to the User to make sure the vectors are of the desired length
    suppressWarnings({
      # for the axis, however, we enforce the same axis across all series named
      # in the group.  We can't stop the user from changing the axis of one or more
      # series later, but at least we can control for some mistakes here
      series$options$axis <- rep(match.arg(axis, c("y", "y2")), 
                      			 			length.out = l)[1]
      if (!is.null(stepPlot)) 
				series$options$stepPlot <- rep(stepPlot, length.out = l)[i]

      if (!is.null(fillGraph)) 
				series$options$fillGraph <- rep(fillGraph, length.out = l)[i]
      
			if (!is.null(drawPoints)) 
				series$options$drawPoints <- rep(drawPoints, length.out = l)[i]

      if (!is.null(pointSize)) 
				series$options$pointSize <- rep(pointSize, length.out = l)[i]
      
			if (!is.null(strokeWidth)) 
				series$options$strokeWidth <- rep(strokeWidth, length.out = l)[i]
      
			if (!is.null(strokePattern)) 
				series$options$strokePattern <- rep(resolveStrokePattern(strokePattern), length.out = l)[i]
      
			if (!is.null(strokeBorderWidth)) 
				series$options$strokeBorderWidth <- rep(strokeBorderWidth, length.out = l)[i]
      
			if (!is.null(strokeBorderColor)) 
				series$options$strokeBorderColor <- rep(strokeBorderColor, length.out = l)[i]
     
      # one can use this to pass a group plotter or any combination of individual series plotters 
      series$options$plotter <- JS(plotter)
    })
    
    # KEY!  Adding a group designator to aid in group plotters
    # By concatenating the names provided in the name age, it becomes
    # a unique identifier than won't be duplicated unless the entire group of names
    # passed gets re-passed together a second time, which would obviously override
    # the first set of options
    series$options$group <- paste0(name, collapse = "")
  
    seriesData <- data[[series$name]]
    
    # grab the colors for the series being processed
  if (!is.null(dygraph$x$attrs$colors)) {
      currColors <- colors[names(colors) %in% name[i]]
      
			if (!is.null(color)) 
				currColors[[series$name]] <- color[i]
      
      colors <- colors[!names(colors) %in% name[i]]
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
    
    # get whatever options might have previously existed for the series, then merge
    base <- attrs$series[[series$label]]
	 	series$options <- mergeLists(base, series$options)
    
    # set options
    attrs$series[[series$label]] <- series$options
    
    # set attrs
    dygraph$x$attrs <- attrs
    
    # set point shape
    if (!is.null(pointShape[i])) {
      shapes <- c("dot", "triangle", "square", "diamond", "pentagon",
                  "hexagon", "circle", "star", "plus", "ex")

      if (!is.element(pointShape[i], shapes)) {
        stop("Invalid value for pointShape parameter. ",
             "Should be one of the following: ",
             "'dot', 'triangle', 'square', 'diamond', 'pentagon', ",
             "'hexagon', 'circle', 'star', 'plus' or 'ex'")
      }
  
      if (pointShape[i] != "dot")
        dygraph$x$pointShape[[series$label]] <- rep(pointShape, length.out = l)
      
    }
    
    # add data
    dygraph$x$data[[length(dygraph$x$data) + 1]] <- seriesData
  }  
  
  # return modified dygraph
  dygraph
}

