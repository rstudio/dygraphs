#' CSS for dygraph labels and legend
#' 
#' Apply custom CSS to the text drawn within a dygraph. See the 
#' \href{http://dygraphs.com/css.html}{CSS documentation} on the dygraphs 
#' website for additional details on which styles are available.
#' 
#' @param dygraph Dygraph to add CSS styles to
#' @param css Path to css file to be used for styling textual elements of the 
#'   graph.
#'   
#' @return dygraph with additional CSS styles
#'   
#' @note See the 
#'   \href{http://jjallaire.github.io/dygraphs/gallery-css-styling.html}{CSS 
#'   Styling} article on the dygraphs for R website for additional details.
#'   
#'   Note that CSS styles are global so will affect all dygraphs on a given web
#'   page. This also implies that for a page with multiple plots you only need
#'   to specify styles for the first one (alternatively you can just add them
#'   directly to the page by other means).
#'   
#' @export
dyCSS <- function(dygraph, css) {
  
  # read css
  dygraph$x$css <- paste(readLines(css, warn = FALSE), collapse = "\n")
  
  # return modified dygraph
  dygraph
}