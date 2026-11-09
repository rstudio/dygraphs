# The dyCrosshair plugin draws a crosshair line over the point closest to the mouse when the user hovers over the graph. It has a "direction" option which is provided in the R wrapper function and then forwarded to the plugin using the "options" argument to dyPlugin.

The dyCrosshair plugin draws a crosshair line over the point closest to
the mouse when the user hovers over the graph. It has a "direction"
option which is provided in the R wrapper function and then forwarded to
the plugin using the "options" argument to dyPlugin.

## Usage

``` r
dyCrosshair(dygraph, direction = c("both", "horizontal", "vertical"))
```

## Arguments

- dygraph:

  Dygraph to add plugin to

- direction:

  Crosshair direction. Valid options are: "both", "horizontal",
  "vertical"

## Value

Dygraph with Crosshair plugin enabled

## Examples

``` r
library(dygraphs)
dygraph(ldeaths) %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical")

{"x":{"attrs":{"labels":["month","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z"],[3035,2552,2704,2554,2014,1655,1721,1524,1596,2074,2199,2512,2933,2889,2938,2497,1870,1726,1607,1545,1396,1787,2076,2837,2787,3891,3179,2011,1636,1580,1489,1300,1356,1653,2013,2823,3102,2294,2385,2444,1748,1554,1498,1361,1346,1564,1640,2293,2815,3137,2679,1969,1870,1633,1529,1366,1357,1570,1535,2491,3084,2605,2573,2143,1693,1504,1461,1354,1333,1492,1781,1915]],"plugins":{"Crosshair":{"direction":"vertical"}}},"evals":["attrs.interactionModel"],"jsHooks":[]}
```
