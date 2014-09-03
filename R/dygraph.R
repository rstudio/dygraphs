
# TODO: Overcome need for Depends on zoo and xts
# TODO: group parameter for linked charts
# TODO: special syntax for y2?
# TODO: how is missing data handled?
# TODO: anything else where the user needs special help 
#   - js formatter functions?

#' Interactive plot for time series data
#' 
#' R interface to interactive time series plotting using the 
#' \href{http://dygraphs.com}{dygraphs} JavaScript library.
#' 
#' @param data Time series data (must be an \link[xts]{xts} object or object 
#'   which is covertible to \code{xts}).
#' @param title Main plot title (optional)
#' @param series Series definition (or list of series definitions) created using
#'   the \code{\link{dySeries}} function. Series can be bound positionally or 
#'   explicity using the \code{name} parameter of \code{dySeries}.
#' @param axes Axis definition (or list of axis definitions) created using the 
#'   the \code{\link{dyAxis}} function.
#' @param options Additional options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
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
                    options = list(),
                    width = NULL, 
                    height = NULL) {
  
  # convert to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity 
  periodicity <- periodicity(data)
  
  # convert time string we can pass to javascript Date function and
  # extract core data from xts object
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  data <- coredata(data)
  
  # calculate column names
  colNames <- colnames(data)
  if (is.null(colNames))
    colNames <- paste("V", 1:ncol(data), sep="")
  
  # merge time into data then strip the metadata (so that the data is 
  # marshalled as a 2d array into json)
  data <- cbind(time, as.data.frame(data), stringsAsFactors = FALSE)
  data <- unclass(data)
  names(data) <- NULL
  
  # create native dygraph options object
  x <- list()
  x$title <- title
  x$labels <- c(periodicity$label, colNames)
  if (length(colNames) > 1)
    x$legend <- "always"
  x$axes$x <- list() 
   
  # add series
  if (inherits(series, "dygraph.series"))
    series <- list(series)
  if (length(series) > 0) {
    for (i in 1:length(series)) { 
      # copy the series and validate it
      s <- series[[i]]
      if (!inherits(s, "dygraph.series"))
        stop("You must pass only dySeries objects in the series parameter")
        
      # if this is a named series then find it's index
      # and re-bind i to it
      if (!is.null(s$name)) {
        m <- match(s$name, x$labels)
        if (!is.na(m))
          i <- m - 1
      }
      
      # custom label if requested
      if (!is.null(s$label))
        x$labels[[i + 1]] <- s$label
      
      # set series options
      name <- x$labels[[i + 1]]
      x$series[[name]] <- s$options
    }
  }
  
  
  # add axes
  if (inherits(axes, "dygraph.axis"))
    axes <- list(axes)
  if (length(axes) > 0) {
    for (i in 1:length(axes)) {
      
      # copy the axis and validate it
      axis <- axes[[i]]
      if (!inherits(axis, "dygraph.axis"))
        stop("You must pass only dyAxis objects in the axes parameter")
      
      # set axis options
      x[[sprintf("%slabel", axis$name)]] <- axis$label
      x$axes[[axis$name]] <- axis$options  
    }
  }
  
  # merge generic options
  x <- mergeLists(x, options)
  
  # side data we use in javascript
  meta <- list()
  meta$scale <- periodicity$scale
  x$meta <- meta
  
  # add time series data
  x$file <- data
  
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10, browser.fill = TRUE)
  )
}

#' dygraph axis options
#' 
#' Define options for an axis on a dygraph plot.
#' 
#' @param name Axis name ('x', 'y', or 'y2')
#' @param label Label to display for axis (defaults to none)
#' @param ... Per-axis options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Axis options
#'   
#' @export
dyAxis <- function(name, label = NULL, ...) {
  
  if (!name %in% c("x", "y", "y2"))
    stop("Axis name must be 'x', 'y', or 'y2'")
  
  axis <- list()
  axis$name <- name
  axis$label <- label
  axis$options <- list(...)
  structure(axis, class = "dygraph.axis")
}

#' dygraph data series options
#' 
#' Add per-series options to a dygraph plot.
#' 
#' @param name Name of series within dataset (unamed series can be bound 
#'  by order or using the convention V1, V2, etc.).
#' @param label Label to display for series (defaults to name)
#' @param ... Per-series options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @export
dySeries <- function(name = NULL, label = name, ...) {
  series <- list()
  series$name <- name
  series$label <- label
  series$options <- list(...)
  structure(series, class = "dygraph.series")
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
