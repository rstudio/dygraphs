 

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
  if (!xts::is.xts(data)) {
    data <- xts::try.xts(data, error = FALSE)
    if (!xts::is.xts(data))
      stop("Data is not a time series object. Please pass an xts time series ",
           "or another time series dataset that is convertible to xts.")
  }
  
  # check periodicity and use that for the x-axis label
  xLabel <- xts::periodicity(data)$label
  
  # convert time string we can pass to javascript Date function and
  # extract core data from xts object
  time <- format(time(data), format="%a, %d %b %Y %H:%M:%S GMT", tz='GMT')
  data <- zoo::coredata(data)
  
  # calculate column names
  colNames <- colnames(data)
  if (is.null(colNames))
    colNames <- paste("V", 1:ncol(data), sep="")
  
  # merge time into data then strip the metadata (so that the data is 
  # marshalled as a 2d array into json)
  data <- cbind(time, as.data.frame(data), stringsAsFactors = FALSE)
  data <- unclass(data)
  names(data) <- NULL
  
  # convert to native dygraph json options format
  options <- list()
  options$file <- data
  options$labels <- c(xLabel, colNames)
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
