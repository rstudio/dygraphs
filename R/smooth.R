#' Smoothed series for dychart
#' 
#' Add a smoothed series to a dychart.
#' 
#' @inheritParams dySeries
#' 
#' @param confidence \code{TRUE} to display confidence interval around smooth
#' @param level Level of confidence interval to use (defaults to 0.95)
#' @param ... Additional arguments to pass to \code{\link{dySeries}}
#' 
#' @return Dygraph with additional smoothed series
#' 
#' @export
dySmoothedSeries <- function(dychart,
                             name,
                             confidence = TRUE,
                             level = 0.95,
                             label = NULL,
                             color = NULL,
                             axis = "y",
                             ...) {

  # default the label if we need to
  if (is.null(label))
    label <- paste(name, "(fit)")
  
  # create the input data
  inputData <- list(x = attr(dychart$x, "xValues"),
                    y = attr(dychart$x, "data")[[name]])
  
  # create the model
  model <- lm(y ~ x, inputData)
  
  # create the smoothed series
  smoothed <- stats::predict(model, 
                             inputData, 
                             interval = "confidence", 
                             level = level)
  
  # define series names and add fit series 
  lwr <- paste0(name, ".lwr")
  fit <- paste0(name, ".fit")
  upr <- paste0(name, ".upr")
  dychart <- dySeriesData(dychart, fit, smoothed[,1])
  
  # add confidence interval series if requested
  if (confidence) {
    
    dychart <- dySeriesData(dychart, lwr, smoothed[,2])
    dychart <- dySeriesData(dychart, upr, smoothed[,3])
    
    # create multiple series
    dySeries(dychart, c(lwr, fit, upr), label, color, axis, ...)
  
  } else {
    
    # create single series
    dySeries(dychart, fit, label, color, axis, ...)
      
  }
}