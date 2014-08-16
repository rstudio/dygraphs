


#' @importFrom magrittr %>%
#' @export %>%
#' 
#' @export
dygraph <- function(data, title = NULL, width = NULL, height = NULL) {
  
  # verify that it's a time series
  if (!is.ts(data)) 
    stop("Must pass a time-series object as dygraph data argument")
    
  # convert to native dygraph json options format
  options <- list()
  options$file <- list(time(data), c(data))
  options$labels <- c("time", "x")
  options$title <- title
  
  # create widget
  htmlwidgets::createWidget(
    name = "dygraphs",
    x = options,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(viewer.padding = 10, browser.fill = TRUE)
  )
}

#' @export
dyRangeSelector <- function(dygraph,
                            height = 40,  
                            plotFillColor = "#A7B1C4", 
                            plotStrokeColor = "#A7B1C4") {
  selector <- list()
  selector$showRangeSelector <- TRUE
  selector$rangeSelectorHeight <- height
  selector$rangeSelectorPlotFillColor <- plotFillColor
  selector$rangeSelectorPlotStrokeColor <- plotStrokeColor
  dygraph$x <- append(dygraph$x, selector)
  dygraph
}

#' @export
dyRoll <- function(dygraph, rollPeriod = 1, showRoller = FALSE) {
  roll <- list()
  roll$rollPeriod = rollPeriod
  roll$showRoller = showRoller
  dygraph$x <- append(dygraph$x, roll)
  dygraph
}

#' @export
dyAxis <- function(dygraph, name, label) {
  axis <- list()
  axis[[sprintf("%slabel",name)]] <- label
  dygraph$x <- append(dygraph$x, axis)
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
