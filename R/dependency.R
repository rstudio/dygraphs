#' Add external assets as a dygraph dependency
#'
#' @param dygraph Dygraph to add dependency to
#' @param dependency An HTML dependency
#'
#' @return A dygraph with the specified dependency added.
#'
#' @export
dyDependency <- function(dygraph, dependency) {
  if (is.null(dygraph$dependencies)) {
    dygraph$dependencies <- list()
  }
  dygraph$dependencies[[length(dygraph$dependencies) + 1]] <- dependency

  # return dygraph
  dygraph
}

#' Include a dygraph plugin
#'
#' @param dygraph Dygraph to add plugin to
#' @param name Name of plugin
#' @param path Path to plugin JavaScript file
#' @param options Named list of options to pass to plugin constructor
#' @param version Plugin version (e.g. version of package which provides the plugin)
#'
#' @importFrom htmltools htmlDependency
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
#' create an R wrapper for it. See
#' \href{https://rstudio.github.io/dygraphs/gallery-plugins.html}{https://rstudio.github.io/dygraphs/gallery-plugins.html}
#' for details on how to do this.
#'
#' @export
dyPlugin <- function(dygraph, name, path, options = list(), version = "1.0") {
  path <- normalizePath(path)
  pluginDependency <- htmlDependency(paste0("Dygraph.Plugins.", name),
                                     version,
                                     src = dirname(path),
                                     script = basename(path),
                                     all_files = FALSE)
  dygraph <- dyDependency(dygraph, pluginDependency)

  # add the plugin and it's options (will be evaluated by renderValue)
  if (is.null(dygraph$x$plugins)) {
    dygraph$x$plugins <- list()
  }
  if (length(options) == 0) {
    options <- JS("{}")
  }
  dygraph$x$plugins[[name]] <- options

  # return dygraph
  dygraph
}

#' Include a dygraph plotter
#'
#' @param dygraph Dygraph to add plotter to
#' @param name Name of plotter
#' @param path Path to plotter JavaScript file
#' @param version Plotter version (e.g. version of package which provides the plotter)
#'
#' @importFrom htmltools htmlDependency
#' @return A dygraph with the specified plotter enabled.
#'
#' @export
dyPlotter <- function(dygraph, name, path, version = "1.0") {
  path <- normalizePath(path)
  plotterDependency <- htmlDependency(paste0("Dygraph.Plotters.", name),
                                      version,
                                      src = dirname(path),
                                      script = basename(path),
                                      all_files = FALSE)
  dygraph <- dyDependency(dygraph, plotterDependency)

  dygraph$x$plotter <- name

  # return dygraph
  dygraph
}

#' Include a dygraph data handler
#'
#' @param dygraph Dygraph to add data handler to
#' @param name Name of data handler
#' @param path Path to data handler JavaScript file
#' @param version Data handler version (e.g. version of package which provides the
#' data handler)
#'
#' @importFrom htmltools htmlDependency
#' @return A dygraph with the specified data handler enabled.
#'
#' @export
dyDataHandler <- function(dygraph, name, path, version = "1.0") {
  path <- normalizePath(path)
  dataHandlerDependency <- htmlDependency(paste0("Dygraph.DataHandlers.", name),
                                          version,
                                          src = dirname(path),
                                          script = basename(path),
                                          all_files = FALSE)
  dygraph <- dyDependency(dygraph, dataHandlerDependency)

  dygraph$x$dataHandler <- name

  # return dygraph
  dygraph
}
