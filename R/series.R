
#' dygraph data series options
#' 
#' Add per-series options to a dygraph plot.
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
#' @param ... Per-series options to pass directly to dygraphs (see the 
#'   \href{http://dygraphs.com/options.html}{dygraphs documentation} for 
#'   additional details).
#'   
#' @return Series options
#'   
#' @export
dySeries <- function(name = NULL, 
                     label = NULL,
                     color = NULL,
                     axis = "y", 
                     ...) {
  
  # ensure that name is either NULL or of length 1 or 3
  if (!is.null(name) && length(name) != 1 && length(name)  != 3) {
    stop("The name parameter must either be NULL, a single ",
         "character value, or a character value of length 3")
  }
  
  # if a multi-series was specified then ensure we have a label
  if (length(name) == 3 && is.null(label)) {
    stop("You must specify a label when merging 3 input series into ",
         "a common display series")
  }
  
  series <- list()
  series$name <- name
  series$label <- label
  series$color <- color
  series$options <- list(...)
  series$options$axis <- match.arg(axis, c("y", "y2"))
  structure(series, class = "dygraph.series")
}


addSeries <- function (x, series) {
  
  if (inherits(series, "dygraph.series"))
    series <- list(series)
  
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
    
    # resolve colors (if one specified then all must be specified)
    colors <- colors[colors != ""]
    if (length(colors) > 0) {
      if (length(colors) == length(series)) {
        x$colors <- colors
      } else {
        stop("If you specify one custom series color you must specify ",
             "a color for all series")
      }
    }
  }
  x
}


haveCustomSeries <- function(series) {
  TRUE
}


resolveCustomSeries <- function(data, series) {
  data
}
