
#' Modify underlying dataset 
#' 
#' Add data series to or replace series within the underlying dataset
#' being plotted. These functions are used in the creation of custom
#' dySeries functions (e.g. to add smoothing, prediction intervals, or
#' other transformations).
#' 
#' @param dygraph Dygraph object to modify
#' @param name Name of series to add or replace
#' @param values Vector of values for series
#' 
#' @return A new dygraph with the requested data modifications
#' 
#' @export
dyDataAdd <- function(dygraph, name, values) {
  
  # add label
  dygraph$x$attr$labels <- c(dygraph$x$attr$labels, name)
  
  # add data
  dygraph$x$data[[length(dygraph$x$data)+1]] <- values
  
  # return modified dygraph
  dygraph
}


#' @rdname dyDataAdd
#' @export
dyDataReplace <- function(dygraph, name, values) {

  # get index
  col <- which(dygraph$x$attrs$labels == name)
  if (length(col) != 1)
    stop("Series name '", name, "' not found in input data")
  
  # set data
  dygraph$x$data[[col]] <- values
  
  # return modified dygraph
  dygraph
}


