#' dygraph series group
#' 
#' Add a data series group to a dygraph plot. Note that options will use the default 
#' global setting (as determined by \code{\link{dyOptions}}) when not specified 
#' explicitly. Importantly, any dySeries options passed can be passed as a vector of values 
#' and will be replicated across all series named as part of the group. If arguments different in
#' length than the number of serie named, then the argument vector will be 
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
#' @param color Colors for series. These can be of the form "#AABBCC" or 
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
#'   dyGroup(c("mdeaths", "ldeaths"), drawPoints = TRUE, colors = c("blue", "green")) %>%
#'   dySeries("fdeaths", stepPlot = TRUE, color = "red")   
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

  if (length(plotter)>1)
    stop('pass only a single plotter option')
  
  
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
  if (length(dygraph$x$attrs$stackedGraph)>0) {
    if (dygraph$x$attrs$stackedGraph) warning(
      "dyGroup is incompatible with stackedGraph... stackedGraph now FALSE")
    dygraph$x$attrs$stackedGraph <- FALSE;
  }
   
  # Resolve stemPlot into a custom plotter if necessary
  plotter <- resolveStemPlot(stemPlot, plotter)
  
  if (!is.null(pointShape))
    dygraph$x$pointShape <- list()
   
  l<-length(name)
  
  # repeat (most of) the steps from dySeries, just in a loop 
  for (i in 1:l) {
    # copy attrs for modification
    attrs <- dygraph$x$attrs
    
    # create series object
    series <- list()
    series$name <- name[i]
    
    # take the passed options and extend to the length of the name vector; it's
    # up to the User to make sure the vectors are of the desired length
    suppressWarnings({
      series$options$axis               <- rep(match.arg(axis, c("y", "y2")), 
                                               length.out = l)[i]
      series$options$stepPlot           <- rep(stepPlot, length.out = l)[i]
      series$options$fillGraph          <- rep(fillGraph, length.out = l)[i]
      series$options$drawPoints         <- rep(drawPoints, length.out = l)[i]
      series$options$pointSize          <- rep(pointSize, length.out = l)[i]
      series$options$strokeWidth        <- rep(strokeWidth, length.out = l)[i]
      series$options$strokePattern      <- rep(resolveStrokePattern(strokePattern), 
                                               length.out = l)[i]
      series$options$strokeBorderWidth  <- rep(strokeBorderWidth, length.out = l)[i]
      series$options$strokeBorderColor  <- rep(strokeBorderColor, length.out = l)[i]
     
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
    
    # default the label if we need to
    if (is.null(series$label))
      series$label <- series$name  
    
    # add label
    attrs$labels <- c(attrs$labels, series$label)
    
    # set options
    attrs$series[[series$label]] <- series$options
    
    # add color if specified 
    if (!is.null(color))
      attrs$colors <- as.list(c(attrs$colors, 
                          rep(color, length.out = l)))
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
  
      if (pointShape[i] != "dot") {
        dygraph$x$pointShape[[series$label]] <- rep(pointShape, length.out = l)
      }
    }
    
    # add data
    dygraph$x$data[[length(dygraph$x$data) + 1]] <- seriesData
  }  
  
  # return modified dygraph
  dygraph
}

# Dygraph.prototype.computeYAxisRanges_ = function(extremes) {
#   var isNullUndefinedOrNaN = function(num) {
#     return isNaN(parseFloat(num));
#   };
#   var numAxes = this.attributes_.numAxes();
#   var ypadCompat, span, series, ypad;
#   
#   var p_axis;
# 
#   // Compute extreme values, a span and tick marks for each axis.
#   for (var i = 0; i < numAxes; i++) {
#     var axis = this.axes_[i];
#     var logscale = this.attributes_.getForAxis("logscale", i);
#     var includeZero = this.attributes_.getForAxis("includeZero", i);
#     var independentTicks = this.attributes_.getForAxis("independentTicks", i);
#     series = this.attributes_.seriesForAxis(i);
# 
#     // Add some padding. This supports two Y padding operation modes:
#     //
#     // - backwards compatible (yRangePad not set):
#     //   10% padding for automatic Y ranges, but not for user-supplied
#     //   ranges, and move a close-to-zero edge to zero except if
#     //   avoidMinZero is set, since drawing at the edge results in
#     //   invisible lines. Unfortunately lines drawn at the edge of a
#     //   user-supplied range will still be invisible. If logscale is
#     //   set, add a variable amount of padding at the top but
#     //   none at the bottom.
#     //
#     // - new-style (yRangePad set by the user):
#     //   always add the specified Y padding.
#     //
#     ypadCompat = true;
#     ypad = 0.1; // add 10%
#     if (this.getNumericOption('yRangePad') !== null) {
#       ypadCompat = false;
#       // Convert pixel padding to ratio
#       ypad = this.getNumericOption('yRangePad') / this.plotter_.area.h;
#     }
# 
#     if (series.length === 0) {
#       // If no series are defined or visible then use a reasonable default
#       axis.extremeRange = [0, 1];
#     } else {
#       // Calculate the extremes of extremes.
#       var minY = Infinity;  // extremes[series[0]][0];
#       var maxY = -Infinity;  // extremes[series[0]][1];
#       var extremeMinY, extremeMaxY;
# 
#       for (var j = 0; j < series.length; j++) {
#         // this skips invisible series
#         if (!extremes.hasOwnProperty(series[j])) continue;
# 
#         // Only use valid extremes to stop null data series' from corrupting the scale.
#         extremeMinY = extremes[series[j]][0];
#         if (extremeMinY !== null) {
#           minY = Math.min(extremeMinY, minY);
#         }
#         extremeMaxY = extremes[series[j]][1];
#         if (extremeMaxY !== null) {
#           maxY = Math.max(extremeMaxY, maxY);
#         }
#       }
# 
#       // Include zero if requested by the user.
#       if (includeZero && !logscale) {
#         if (minY > 0) minY = 0;
#         if (maxY < 0) maxY = 0;
#       }
# 
#       // Ensure we have a valid scale, otherwise default to [0, 1] for safety.
#       if (minY == Infinity) minY = 0;
#       if (maxY == -Infinity) maxY = 1;
# 
#       span = maxY - minY;
#       // special case: if we have no sense of scale, center on the sole value.
#       if (span === 0) {
#         if (maxY !== 0) {
#           span = Math.abs(maxY);
#         } else {
#           // ... and if the sole value is zero, use range 0-1.
#           maxY = 1;
#           span = 1;
#         }
#       }
# 
#       var maxAxisY, minAxisY;
#       if (logscale) {
#         if (ypadCompat) {
#           maxAxisY = maxY + ypad * span;
#           minAxisY = minY;
#         } else {
#           var logpad = Math.exp(Math.log(span) * ypad);
#           maxAxisY = maxY * logpad;
#           minAxisY = minY / logpad;
#         }
#       } else {
#         maxAxisY = maxY + ypad * span;
#         minAxisY = minY - ypad * span;
# 
#         // Backwards-compatible behavior: Move the span to start or end at zero if it's
#         // close to zero, but not if avoidMinZero is set.
#         if (ypadCompat && !this.getBooleanOption("avoidMinZero")) {
#           if (minAxisY < 0 && minY >= 0) minAxisY = 0;
#           if (maxAxisY > 0 && maxY <= 0) maxAxisY = 0;
#         }
#       }
#       axis.extremeRange = [minAxisY, maxAxisY];
#     }
#     if (axis.valueWindow) {
#       // This is only set if the user has zoomed on the y-axis. It is never set
#       // by a user. It takes precedence over axis.valueRange because, if you set
#       // valueRange, you'd still expect to be able to pan.
#       axis.computedValueRange = [axis.valueWindow[0], axis.valueWindow[1]];
#     } else if (axis.valueRange) {
#       // This is a user-set value range for this axis.
#       var y0 = isNullUndefinedOrNaN(axis.valueRange[0]) ? axis.extremeRange[0] : axis.valueRange[0];
#       var y1 = isNullUndefinedOrNaN(axis.valueRange[1]) ? axis.extremeRange[1] : axis.valueRange[1];
#       if (!ypadCompat) {
#         if (axis.logscale) {
#           var logpad = Math.exp(Math.log(span) * ypad);
#           y0 *= logpad;
#           y1 /= logpad;
#         } else {
#           span = y1 - y0;
#           y0 -= span * ypad;
#           y1 += span * ypad;
#         }
#       }
#       axis.computedValueRange = [y0, y1];
#     } else {
#       axis.computedValueRange = axis.extremeRange;
#     }
#     
#     
#     if (independentTicks) {
#       axis.independentTicks = independentTicks;
#       var opts = this.optionsViewForAxis_('y' + (i ? '2' : ''));
#       var ticker = opts('ticker');
#       axis.ticks = ticker(axis.computedValueRange[0],
#               axis.computedValueRange[1],
#               this.plotter_.area.h,
#               opts,
#               this);
#       // Define the first independent axis as primary axis.
#       if (!p_axis) p_axis = axis;
#     }
#   }
#   if (p_axis === undefined) {
#     throw ("Configuration Error: At least one axis has to have the \"independentTicks\" option activated.");
#   }
#   // Add ticks. By default, all axes inherit the tick positions of the
#   // primary axis. However, if an axis is specifically marked as having
#   // independent ticks, then that is permissible as well.
#   for (var i = 0; i < numAxes; i++) {
#     var axis = this.axes_[i];
#     
#     if (!axis.independentTicks) {
#       var opts = this.optionsViewForAxis_('y' + (i ? '2' : ''));
#       var ticker = opts('ticker');
#       var p_ticks = p_axis.ticks;
#       var p_scale = p_axis.computedValueRange[1] - p_axis.computedValueRange[0];
#       var scale = axis.computedValueRange[1] - axis.computedValueRange[0];
#       var tick_values = [];
#       for (var k = 0; k < p_ticks.length; k++) {
#         var y_frac = (p_ticks[k].v - p_axis.computedValueRange[0]) / p_scale;
#         var y_val = axis.computedValueRange[0] + y_frac * scale;
#         tick_values.push(y_val);
#       }
# 
#       axis.ticks = ticker(axis.computedValueRange[0],
#                           axis.computedValueRange[1],
#                           this.plotter_.area.h,
#                           opts,
#                           this,
#                           tick_values);
#     }
#   }
# };

# Dygraph.stackPoints_ = function(
#     points, cumulativeYval, seriesExtremes, fillMethod) {
#   var lastXval = null;
#   var prevPoint = null;
#   var nextPoint = null;
#   var nextPointIdx = -1;
# 
#   // Find the next stackable point starting from the given index.
#   var updateNextPoint = function(idx) {
#     // If we've previously found a non-NaN point and haven't gone past it yet,
#     // just use that.
#     if (nextPointIdx >= idx) return;
# 
#     // We haven't found a non-NaN point yet or have moved past it,
#     // look towards the right to find a non-NaN point.
#     for (var j = idx; j < points.length; ++j) {
#       // Clear out a previously-found point (if any) since it's no longer
#       // valid, we shouldn't use it for interpolation anymore.
#       nextPoint = null;
#       if (!isNaN(points[j].yval) && points[j].yval !== null) {
#         nextPointIdx = j;
#         nextPoint = points[j];
#         break;
#       }
#     }
#   };
# 
#   for (var i = 0; i < points.length; ++i) {
#     var point = points[i];
#     var xval = point.xval;
#     if (cumulativeYval[xval] === undefined) {
#       cumulativeYval[xval] = 0;
#     }
# 
#     var actualYval = point.yval;
#     if (isNaN(actualYval) || actualYval === null) {
#       if(fillMethod == 'none') {
#         actualYval = 0;
#       } else {
#         // Interpolate/extend for stacking purposes if possible.
#         updateNextPoint(i);
#         if (prevPoint && nextPoint && fillMethod != 'none') {
#           // Use linear interpolation between prevPoint and nextPoint.
#           actualYval = prevPoint.yval + (nextPoint.yval - prevPoint.yval) *
#               ((xval - prevPoint.xval) / (nextPoint.xval - prevPoint.xval));
#         } else if (prevPoint && fillMethod == 'all') {
#           actualYval = prevPoint.yval;
#         } else if (nextPoint && fillMethod == 'all') {
#           actualYval = nextPoint.yval;
#         } else {
#           actualYval = 0;
#         }
#       }
#     } else {
#       prevPoint = point;
#     }
# 
#     var stackedYval = cumulativeYval[xval];
#     if (lastXval != xval) {
#       // If an x-value is repeated, we ignore the duplicates.
#       stackedYval += actualYval;
#       cumulativeYval[xval] = stackedYval;
#     }
#     lastXval = xval;
# 
#     point.yval_stacked = stackedYval;
# 
#     if (stackedYval > seriesExtremes[1]) {
#       seriesExtremes[1] = stackedYval;
#     }
#     if (stackedYval < seriesExtremes[0]) {
#       seriesExtremes[0] = stackedYval;
#     }
#   }
# };
