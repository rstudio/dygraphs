 
#' @export
dygraph <- function(data, 
                    title = NULL, 
                    xaxis = dyAxis(), 
                    yaxis = dyAxis(),
                    interaction = dyInteraction(), 
                    theme = dyTheme(),
                    width = NULL, 
                    height = NULL) {
  
  # verify that it's a time series
  if (!is.ts(data)) 
    stop("Must pass a time-series object as dygraph data argument")
    
  # convert to native dygraph json options format
  options <- list()
  options$file <- list(time(data), c(data))
  axisLegend <- function(axis, default) 
    ifelse(is.null(axis$legend), default, axis$legend)
  options$labels <- c(axisLegend(yaxis,"time"), axisLegend(xaxis, "x"))
  options$title <- title
  
  # merge axis
  mergeAxis <- function(name, axis) {
    options[[sprintf("%slabel", name)]] <<- axis$label 
  }
  mergeAxis("x", xaxis)
  mergeAxis("y", yaxis)
  
  # merge various other options
  options <- append(options, interaction)
  options <- append(options, theme)
  
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
dyAxis <- function(label = NULL, legend = label) {
  axis <- list()
  axis$label <- label
  axis$legend <- legend
  axis
}

#' @export
dyInteraction <- function(legend = "onmouseover",
                          showRangeSelector = FALSE,
                          showLabelsOnHighlight = TRUE,
                          showRoller = FALSE) {
  interaction <- list()
  interaction$legend <- legend
  interaction$showRangeSelector <- showRangeSelector
  interaction$showLabelsOnHighlight <- showLabelsOnHighlight
  interaction$showRoller <- showRoller
  interaction
}

#' @export
dyTheme <- function(rangeSelectorHeight = 40,
                    rangeSelectorPlotFillColor = "#A7B1C4",
                    rangeSelectorPlotStrokeColor =  "#A7B1C4") {
  theme <- list()
  theme$rangeSelectorHeight <- rangeSelectorHeight
  theme$rangeSelectorPlotFillColor <- rangeSelectorPlotFillColor
  theme$rangeSelectorPlotStrokeColor <- rangeSelectorPlotStrokeColor
  theme
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
