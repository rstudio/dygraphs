#' Callbacks for dygraph events
#' 
#' Set JavaScript callbacks for various dygraph events. See the 
#' \href{http://dygraphs.com/options.html}{dygraph options} reference for 
#' additional details on the signature of each callback.
#' 
#' @param dygraph Dygraph to add callbacks to
#' @param clickCallback A function to call when the canvas is clicked.
#' @param drawCallback When set, this callback gets called every time the 
#'   dygraph is drawn. This includes the initial draw, after zooming and 
#'   repeatedly while panning.
#' @param highlightCallback When set, this callback gets called every time a new
#'   point is highlighted.
#' @param pointClickCallback A function to call when a data point is clicked. 
#'   and the point that was clicked.
#' @param underlayCallback When set, this callback gets called before the chart 
#'   is drawn.
#' @param unhighlightCallback When set, this callback gets called every time the
#'   user stops highlighting any point by mousing out of the graph.
#' @param zoomCallback A function to call when the zoom window is changed 
#'   (either by zooming in or out).
#' @param drawHighlightPointCallback Draw a custom item when a point is 
#'   highlighted. Default is a small dot matching the series color. This method 
#'   should constrain drawing to within pointSize pixels from (cx, cy)
#' @param drawPointCallback Draw a custom item when drawPoints is enabled. 
#'   Default is a small dot matching the series color. This method should 
#'   constrain drawing to within pointSize pixels from (cx, cy).
#' @param annotationClickHandler JavaScript function to call when an annotation
#'   is clicked. This can also be specified on a per-annotation basis.
#' @param annotationMouseOverHandler JavaScript function to call when the mouse
#'   hovers over an annotation. This can also be specified on a per-annotation
#'   basis.
#' @param annotationMouseOutHandler JavaScript function to call when the mouse
#'   exits an annotation. This can also be specified on a per-annotation basis.
#' @param annotationDblClickHandler JavaScript function to call when an
#'   annotation is double clicked. This can also be specified on a
#'   per-annotation basis.
#'   
#' @return Dygraph with callbacks
#'   
#' @export
dyCallbacks <- function(dygraph,
                        clickCallback = NULL,
                        drawCallback = NULL,
                        highlightCallback = NULL,
                        pointClickCallback = NULL,
                        underlayCallback = NULL,
                        unhighlightCallback = NULL,
                        zoomCallback = NULL,
                        drawHighlightPointCallback = NULL,
                        drawPointCallback = NULL,
                        annotationClickHandler = NULL,
                        annotationMouseOverHandler = NULL,
                        annotationMouseOutHandler = NULL,
                        annotationDblClickHandler = NULL) {
  
  callbacks <- list()
  callbacks$clickCallback <- clickCallback
  callbacks$drawCallback <- drawCallback
  callbacks$highlightCallback <- highlightCallback
  callbacks$pointClickCallback <- pointClickCallback
  callbacks$underlayCallback <- underlayCallback
  callbacks$unhighlightCallback <- unhighlightCallback
  callbacks$zoomCallback <- zoomCallback
  callbacks$drawHighlightPointCallback <- drawHighlightPointCallback
  callbacks$drawPointCallback <- drawPointCallback
  callbacks$annotationClickHandler <- annotationClickHandler
  callbacks$annotationMouseOverHandler <- annotationMouseOverHandler
  callbacks$annotationMouseOutHandler <- annotationMouseOutHandler
  callbacks$annotationDblClickHandler <- annotationDblClickHandler
  
  dygraph$x$attrs <- mergeLists(dygraph$x$attrs, callbacks)
  
  dygraph
}