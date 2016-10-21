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

#' dyUnzoom
#'
#' @inheritParams dyPlugin
#'
#' @return A dygraph with the specified plugin enabled.
#'
#' @details The dyUnzoom plugin adds an "Unzoom" button to the graph when it's displaying
#' in a zoomed state (this is a bit more discoverable than the default double-
#' click gesture for unzooming). Note that this plugin has no options (see 
#' below for an example with options).
#'
#' @examples 
#' library(dygraphs)
#' dygraph(mdeaths) %>%
#'   dyUnzoom()
#'
#' @export
dyUnzoom <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("examples/plugins/unzoom.js", package = "dygraphs")
  )
}

#' dyCrosshair
#'
#' @inheritParams dyPlugin
#' @param direction Direction for crosshairs. Defaults to 'both'. Valid arguments are 
#  'both', 'horizontal', and 'vertical'.
#'
#' @return A dygraph with the specified plugin enabled.
#'
#' @details The dyCrosshair plugin draws a crosshair line over the point closest to the
#' mouse when the user hovers over the graph. It has a "direction" option which
#' is provided in the R wrapper function and then forwarded to the plugin using
#' the "options" argument to dyPlugin.
#'
#' @examples 
#' library(dygraphs)
#' dygraph(mdeaths) %>%
#'   dyCrosshair()
#'
#' @export
dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("examples/plugins/crosshair.js", package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

#' dySliderInput
#'
#' @inheritParams dyPlugin
#' @param color Color to draw slider. Defaults to 'red'.
#' @param strokePattern Line type for slider. Defaults to 'dashed'. Valid arguments are
#' 'dashed', 'solid', 'dotted', and 'dotdash'.
#' @param animate 'TRUE' to show simple animation controls with default settings; 
#' 'FALSE' not to; or a custom settings list, such as those created using 'animationOptions'.
#'
#' @return A dygraph with the specified plugin enabled.
#'
#' @details The dySliderInput plugin turns the dyDygraph into a slider input widget. The
#' user can click on a point along the graph and the graph will place a vertical
#' line on the graph. The user can also use the animation options to scroll
#' through points along the graph.
#'
#' @examples 
#' library(dygraphs)
#' dygraph(mdeaths) %>%
#'   dySliderInput()
#'
#' @importFrom grDevices col2rgb
#' @importFrom shiny icon
#' @importFrom shiny animationOptions
#'
#' @export
dySliderInput <- function(dygraph, color = 'red', strokePattern = c('dashed', 'solid', 'dotted', 'dotdash'), animate = FALSE) {
  # process args
  stopifnot(length(color)==1)
  col <- paste(col2rgb(color)[,1], collapse=',')
  alpha <- (col2rgb(color, alpha=TRUE)[4,1] / 255)
  if (identical(animate, TRUE))
    animate <- animationOptions()
  if (identical(animate, FALSE))
    animate <- NULL
  if (!is.null(animate)) {
    if (is.null(animate$playButton)) 
      animate$playButton <- as.character(icon("play", lib = "glyphicon"))
    if (is.null(animate$pauseButton)) 
      animate$pauseButton <- as.character(icon("pause", lib = "glyphicon"))
  }
  # add plugin
  dyPlugin(
    dygraph = dygraph,
    name = "SliderInput",
    path = system.file("examples/plugins/sliderinput.js", package = "dygraphs"),
    options = list(strokeStyle = paste0(col, ',', alpha), strokePattern=resolveStrokePattern(match.arg(strokePattern)), animate = animate)
  )
}
