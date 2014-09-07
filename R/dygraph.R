
# TODO: need a colors argument on dyOptions
# TODO: support for annotations
# TODO: support of other data input types? (e.g. formula, x, y, etc.)
# TODO: consider supporting x,y,main,xlab,ylab for consistency with plot function
# TODO: improved default CSS/fonts (for viewer pane only?)
# TODO: overcome xts imports/s3 issues
# TODO: does the use of dySeries automatically discard other columns?
# TODO: built-in support for smoothing (regressions) with error bars
#         -- look at loess and lm/predict, e.g.
#               predict(lm(dist ~ speed, cars), cars, interval = "confidence")
#         -- points + line with error bar for future
#         -- dySmooth as series type
#         -- allow dySeries to return multiple data series (via S3?)

# TODO: docs and examples

#' Interactive plot for time series data
#' 
#' R interface to interactive time series plotting using the 
#' \href{http://dygraphs.com}{dygraphs} JavaScript library.
#' 
#' @param data Time series data (must be an \link[xts]{xts} object or an object 
#'   which is covertible to \code{xts}).
#' @param title Main plot title (optional)
#' @param series Series definition (or list of series definitions) created using
#'   the \code{\link{dySeries}} function. Series can be bound positionally or 
#'   explicity using the \code{name} parameter of \code{dySeries}.
#' @param axes Axis definition (or list of axis definitions) created using the 
#'   the \code{\link{dyAxis}} function.
#' @param interaction Interactions (or list of interactions) created using the
#'   \code{\link{dyInteraction}} function.
#' @param options Options (or list of options) created using the
#'   \code{\link{dyOptions}} function.
#' @param group Group to associate this plot with. The x-axis zoom level of 
#'   plots within a group is automatically synchronized.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#'   
#' @return Interactive dygraph plot
#'   
#' @export
dygraph <- function(data, 
                    title = NULL,
                    series = list(),
                    axes = list(),
                    interaction = list(),
                    options = list(),
                    group = NULL,
                    width = NULL, 
                    height = NULL) {
  
  # allow series, axes, interaction, and options to be specified as single elements
  if (inherits(series, "dygraph.series"))
    series <- list(series)
  if (inherits(axes, "dygraph.axis"))
    axes <- list(axes)
  if (inherits(interaction, "dygraph.interaction"))
    interaction <- list(interaction)
  if (inherits(options, "dygraph.options"))
    options <- list(options)
  
  # convert data to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity 
  periodicity <- periodicity(data)
  
  # convert time string we can pass to javascript Date function and
  # extract core data from xts object
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  data <- zoo::coredata(data)
      
  # convert to a named list
  data <- unclass(as.data.frame(data))
   
  # resolve custom series (aggregates or smoothed versions of input series)
  customBars <- haveCustomBars(series)
  if (customBars) {
    resolved <- resolveCustomBars(data, series)
    data <- resolved$data
    series <- resolved$series
  }
  
  # get column names, merge time into data-frame, then strip the metadata 
  # (so we are marshalled as a 2d array into json)
  colNames <- names(data)
  data <- append(list(time), data)
  names(data) <- NULL
    
  # create native dygraph attrs object
  attrs <- list()
  attrs$title <- title
  attrs$customBars <- customBars
  attrs$labels <- c(periodicity$label, colNames)
  if (length(colNames) > 1)
    attrs$legend <- "always"
  attrs$axes$x <- list() 
   
  # add series, axes, and options
  attrs <- addSeries(attrs, series)
  attrs <- addAxes(attrs, axes)
  attrs <- addInteraction(attrs, interaction)
  attrs <- addOptions(attrs, options)
  
  # add time series data (we do this at the end so we don't pay the
  # price of copying it as we mutate 'attrs' above)
  attrs$file <- data
  
  # create x (dychart attrs + some side data)
  x <- list()
  x$attrs <- attrs
  x$scale <- periodicity$scale
  x$group <- group
  
  # hoist css into side data
  x$css <- attrs$css
  attrs$css <- NULL
  
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10, browser.fill = TRUE)
  )
}

#' @export
dygraphOutput <- function(outputId, width = "100%", height = "400px") {
  htmlwidgets::shinyWidgetOutput(outputId, "dygraphs", width, height)
}

#' @export
renderDygraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, dygraphOutput, env, quoted = TRUE)
}
