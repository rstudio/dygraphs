
# TODO: support for annotations
# TODO: consider using magrittr syntax
#       - if we do this then separate data into it's own x field so 
#         that mergeLists doesn't repeadely copy the data)
# TODO: support of other data input types? (e.g. formula, x, y, etc.)
# TODO: consider supporting x,y,main,xlab,ylab for consistency with plot function
# TODO: improved default CSS/fonts (for viewer pane only?)
# TODO: built-in support for smoothing (regressions) with error bars
#         -- look at loess and lm/predict, e.g.
#               predict(lm(dist ~ speed, cars), cars, interval = "confidence")
#         -- dySmooth as series type
# TODO: custom series types: dyPoints, dyLine, dySmooth, dyPredict
# TODO: consider breaking dyOptions into dyOptions and dyTheme

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
#' @importFrom xts is.xts
#' @importFrom xts as.xts
#' @importFrom xts periodicity
#' @importFrom zoo coredata
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
  
  # convert data to xts
  if (!xts::is.xts(data))
    data <- xts::as.xts(data)
  
  # check periodicity 
  periodicity <- xts::periodicity(data)
  
  # convert time to string we can pass to javascript Date function
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  
  # get data as a named list
  data <- zoo::coredata(data)
  data <- unclass(as.data.frame(data))
   
  # merge time back into list
  timeColumn <- list()
  timeColumn[[periodicity$label]] <- time
  data <- append(timeColumn, data)
  
  # create native dygraph attrs object
  attrs <- list()
  attrs$title <- title
  attrs$labels <- names(data)
  if (length(attrs$labels) > 1)
    attrs$legend <- "always"
  attrs$axes$x <- list() 
   
  # create x (dychart attrs + some side data)
  x <- list()
  x$attrs <- attrs
  x$scale <- periodicity$scale
  x$group <- group
  
  # add data (strip names first so we marshall as a 2d array)
  names(data) <- NULL
  x$attrs$file <- data
  
  # add series
  if (inherits(series, "dygraph.series"))
    series <- list(series)
  for (s in series)
    x <- addSeries(x, s)
    
  # add axes
  if (inherits(axes, "dygraph.axis"))
    axes <- list(axes)
  x$attrs <- addAxes(x$attrs, axes)
  
  # add interaction
  if (inherits(interaction, "dygraph.interaction"))
    interaction <- list(interaction)
  x$attrs <- addInteraction(x$attrs, interaction)

  # add options (hoist css into side data)
  if (inherits(options, "dygraph.options"))
    options <- list(options)
  x$attrs <- addOptions(x$attrs, options)
  x$css <- x$attrs$css
  x$attrs$css <- NULL
   
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
