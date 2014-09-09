
#' Interactive plot for time series data
#' 
#' R interface to interactive time series plotting using the 
#' \href{http://dygraphs.com}{dygraphs} JavaScript library.
#' 
#' @param data Time series data (must be an \link[xts]{xts} object or an object 
#'   which is covertible to \code{xts}).
#' @param main Main plot title (optional)
#' @param xlab X axis label
#' @param ylab Y axis label
#' @param group Group to associate this plot with. The x-axis zoom level of 
#'   plots within a group is automatically synchronized.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#'   
#' @return Interactive dygraph plot
#' 
#' @importFrom magrittr %>%
#' @export %>%     
#'     
#' @export
dygraph <- function(data, main = NULL, xlab = NULL, ylab = NULL,
                    group = NULL, width = NULL, height = NULL) {
  
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
  attrs$title <- main
  attrs$xlabel <- xlab
  attrs$ylabel <- ylab
  attrs$labels <- names(data)
  attrs$legend <- "auto"
  attrs$axes$x <- list() 
   
  # create x (dychart attrs + some side data)
  x <- list()
  x$attrs <- attrs
  x$scale <- periodicity$scale
  x$group <- group
  
  # add attributes required for defining custom series. when a dySeries call
  # is made it places series definition in "manual mode"; in this case we
  # need to save the original data 
  attr(x, "data") <- data
  
  # add data (strip names first so we marshall as a 2d array)
  names(data) <- NULL
  x$data <- data
    
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
