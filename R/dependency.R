#' Add an external JavaScript asset as a dygraph dependency
#'
#' @param dygraph Dygraph to add dependency to
#' @param type Type of dependency (valid options are: 'plugins', 'plotter' or 'dataHandler')
#' @param name Name of dependency
#' @param path Path to dependency JavaScript file
#' @param version Dependency version (e.g. version of package which provides the dependency)
#' @param all.files Whether all files under the path directory are dependency files
#'
#' @return A dygraph with the specified dependency added.
#'
#' @importFrom htmltools htmlDependency
#'
#' @export
dyDependency <- function(dygraph, type, name, path, version = "1.0", all.files = FALSE) {
  # create an html dependency for the js file
  path <- normalizePath(path)
  assetDependency <- htmlDependency(depName(type, name),
                                    version,
                                    src = dirname(path),
                                    script = basename(path),
                                    all_files = all.files)

  # add javascript to the dependencies
  if (is.null(dygraph$dependencies)) {
    dygraph$dependencies <- list()
  }
  dygraph$dependencies[[length(dygraph$dependencies) + 1]] <- assetDependency

  # return dygraph
  dygraph
}

# Make up a htmlDependency name based on asset type and name
depName <- function(type, name) {
  switch(type,
         plugins = paste0("Dygraph.Plugins.", name),
         plotter = paste0("Dygraph.Plotters.", name),
         dataHandler = paste0("Dygraph.DataHandlers.", name))
}

#' Include a dygraph plugin
#'
#' @param dygraph Dygraph to add plugin to
#' @param name Name of plugin
#' @param path Path to plugin JavaScript file
#' @param options Named list of options to pass to plugin constructor
#' @param version Plugin version (e.g. version of package which provides the plugin)
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
#' @export
dyPlugin <- function(dygraph, name, path, options = list(), version = "1.0") {
  dygraph <- dyDependency(dygraph = dygraph,
                          type = 'plugins',
                          name = name,
                          path = path,
                          version = version)
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
#' @return A dygraph with the specified plotter enabled.
#'
#' @export
dyPlotter <- function(dygraph, name, path, version = "1.0") {
  dygraph <- dyDependency(dygraph = dygraph,
                          type = 'plotter',
                          name = name,
                          path = path,
                          version = version)
  dygraph$x$plotter <- name

  # return dygraph
  dygraph
}
