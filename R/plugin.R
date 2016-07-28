#' Include a dygraph plugin
#' 
#' @param dygraph Dygraph to add plugin to
#' @param name Name of plugin
#' @param path Path to plugin JavaScript file
#' @param options Named list of options to pass to plugin constructor
#' @param version Plugin version (e.g. version of package which 
#'   provides the plugin)
#' 
#' @return A dygraph with the specified plugin enabled.
#' 
#' @details 
#' You can use dygraphs plugins to customize the appearance of dygraphs as well
#' as add new interactive behaviors. For additional information on creating
#' dygraphs plugins see the 
#' \href{https://github.com/danvk/dygraphs/tree/master/src/plugins}{dygraphs plugins}
#' documentation.
#
#' Once you've created a dygraphs plugins you can use the dyPlugin function to
#' create an R wrapper for it. See the 
#' \href{https://github.com/rstudio/dygraphs/tree/master/inst/examples/plugins/plugins.R}{example plugins}
#' for details on how to do this.
#' 
#' @importFrom htmltools htmlDependency
#' 
#' @export
dyPlugin <- function(dygraph, name, path, options = list(), version = "1.0") {
  
  # create an html dependency for the plugin js file
  path <- normalizePath(path)
  pluginDependency <- htmlDependency(paste0("Dygraph.Plugins.", name), 
                                     version, 
                                     src = dirname(path), 
                                     script = basename(path),
                                     all_files = FALSE)
  
  # add the plugin javascript to the dependencies
  if (is.null(dygraph$dependencies))
    dygraph$dependencies <- list()
  dygraph$dependencies[[length(dygraph$dependencies) + 1]] <- pluginDependency
  
  # add the plugin and it's options (will be evaluated by renderValue)
  if (is.null(dygraph$x$plugins))
    dygraph$x$plugins <- list()
  if (length(options) == 0)
    options <- JS("{}")
  dygraph$x$plugins[[name]] <- options
  
  # return dygraph
  dygraph
}
