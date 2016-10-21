
# dyPlugin -- Create R interfaces for dygraphs plugins. 
#
# You can use dygraphs plugins to customize the appearance of dygraphs as well
# as add new interactive behaviors. For additional information on creating
# dygraphs plugins see https://github.com/danvk/dygraphs/tree/master/src/plugins
#
# Once you've created a dygraphs plugins you can use the dyPlugin function to
# create an R wrapper for it. 


# The dyUnzoom plugin adds an "Unzoom" button to the graph when it's displaying
# in a zoomed state (this is a bit more discoverable than the default double-
# click gesture for unzooming). Note that this plugin has no options (see 
# below for an example with options).
dyUnzoom <-function(dygraph) {
  dyPlugin(
    dygraph = dygraph,
    name = "Unzoom",
    path = system.file("examples/plugins/unzoom.js", package = "dygraphs")
  )
}


# The dyCrosshair plugin draws a crosshair line over the point closest to the
# mouse when the user hovers over the graph. It has a "direction" option which
# is provided in the R wrapper function and then forwarded to the plugin using
# the "options" argument to dyPlugin.
dyCrosshair <- function(dygraph, direction = c("both", "horizontal", "vertical")) {
  dyPlugin(
    dygraph = dygraph,
    name = "Crosshair",
    path = system.file("examples/plugins/crosshair.js", package = "dygraphs"),
    options = list(direction = match.arg(direction))
  )
}

# Our plugin wrapper functions can now be incorporated directly into a dygraph
# pipeline along with other dygraphs functions:

library(dygraphs)
dygraph(ldeaths) %>% 
  dyRangeSelector() %>% 
  dyUnzoom() %>% 
  dyCrosshair(direction = "vertical") %>%



