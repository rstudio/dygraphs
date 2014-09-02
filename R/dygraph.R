 
#' @importFrom xts is.xts
#' @importFrom xts as.xts
#' @export
dygraph <- function(data, 
                    title = NULL, 
                    xaxis = dyAxis(), 
                    yaxis = dyAxis(),
                    interaction = dyInteraction(), 
                    theme = dyTheme(),
                    width = NULL, 
                    height = NULL) {
  
  
  # convert to xts
  if (!is.xts(data)) 
    data <- as.xts(data)
  
  # convert to native dygraph json options format
  options <- list()
  options$file <- list(format(index(data),"%Y/%m/%d"), coredata(data))
  options$labels <- c("time", "x")
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
dyAxis <- function(label = NULL) {
  axis <- list()
  axis$label <- label
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
dyTheme <- function(titleHeight = 24,
                    xLabelHeight = 18,
                    yLabelWidth = 18,
                    drawGrid = TRUE,
                    rangeSelectorHeight = 40,
                    rangeSelectorPlotFillColor = "#A7B1C4",
                    rangeSelectorPlotStrokeColor =  "#A7B1C4") {
  theme <- list()
  theme$titleHeight <- titleHeight
  theme$xLabelHeight <- xLabelHeight
  theme$yLabelWidth <- yLabelWidth
  theme$drawGrid <- drawGrid
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
