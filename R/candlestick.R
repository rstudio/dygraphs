
#' @rdname Plotters
#' @export
dyCandlestick <- function(dygraph, compress = FALSE) {
  path <- system.file("plotters/candlestick.js", package = "dygraphs")
  path <- normalizePath(path)
  dygraph <- dyPlotter(
    dygraph = dygraph,
    name = "CandlestickPlotter",
    path = path,
    version = "1.0"
  )

  if (compress) {
    path <- system.file("plugins/compress.js", package = "dygraphs")
    path <- normalizePath(path)
    dygraph <- dyDataHandler(
      dygraph = dygraph,
      name = "CompressHandler",
      path = path,
      version = "1.0"
    )
    dygraph <- dyAxis(dygraph, "x", valueFormatter = "
    function(millis) {
      var monthNames = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                        'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
      var scale = this.dataHandler_.compressTitle;
      var date = new Date(millis);
      if (scale == 'yearly')
        return date.getFullYear();
      else if (scale == 'quarterly')
        return moment(millis).fquarter(1);
      else if (scale == 'monthly')
        return monthNames[date.getMonth()] + ', ' + date.getFullYear();
      else if (scale == 'daily' || scale == 'weekly')
        return monthNames[date.getMonth()] + ', ' +
          date.getDate() + ', ' +
          date.getFullYear();
        else
          return date.toLocaleString();
    }")
  }

  # return dygraph
  dygraph
}
