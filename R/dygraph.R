 

#' @importFrom magrittr %>%
#' @export %>%
#' 
#' @export
dygraph <- function(data, title = NULL, width = NULL, height = NULL) {
  
  # convert to xts
  if (!is.xts(data))
    data <- as.xts(data)
  
  # check periodicity and use that for the x-axis label
  xLabel <- periodicity(data)$label
  
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
  
  # convert to native dygraph json options format
  x <- list()
  x$file <- data
  x$title <- title
  x$labels <- c(xLabel, colNames)
 
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
dyAxis <- function(dygraph, name, label = NULL) {
  axis <- list()
  axis[[sprintf("%slabel",name)]] <- label
  dygraph$x <- append(dygraph$x, axis)
  dygraph
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
dyTheme <- function(dygraph,
                    titleHeight = 24,
                    xLabelHeight = 18,
                    yLabelWidth = 18,
                    drawGrid = TRUE,
                    rangeSelectorPlotFillColor = "#A7B1C4",
                    rangeSelectorPlotStrokeColor =  "#A7B1C4") {
  theme <- list()
  theme$titleHeight <- titleHeight
  theme$xLabelHeight <- xLabelHeight
  theme$yLabelWidth <- yLabelWidth
  theme$drawGrid <- drawGrid
  theme$rangeSelectorPlotFillColor <- rangeSelectorPlotFillColor
  theme$rangeSelectorPlotStrokeColor <- rangeSelectorPlotStrokeColor
  dygraph$x <- append(dygraph$x, theme)
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
