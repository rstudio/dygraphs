
#' @export
dygraph <- function(data, title = NULL, ylabel = NULL, width = NULL, height = NULL) {
  
  # verify that it's a time series
  if (!is.ts(data)) 
    stop("Must pass a time-series object as dygraph data argument")
    
  # convert to native dygraph json options format
  options <- list()
  options$file <- list(time(data), c(data))
  options$labels <- c("time", "x")
  options$title <- title
  options$ylabel <- ylabel
   
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = options,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10)
  )
}

#' @export
rangeSelector <- function(dygraph, 
                          height = 40,  
                          plotFillColor = "#A7B1C4", 
                          plotStrokeColor = "#A7B1C4") {
  dygraph$x$showRangeSelector <- TRUE
  dygraph$x$rangeSelectorHeight <- height
  dygraph$x$rangeSelectorPlotFillColor <- plotFillColor
  dygraph$x$rangeSelectorPlotStrokeColor <- plotStrokeColor
  dygraph
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
