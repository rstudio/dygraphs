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

{"x":{"attrs":{"labels":["month","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["1974-01-01T00:00:00.000Z","1974-02-01T00:00:00.000Z","1974-03-01T00:00:00.000Z","1974-04-01T00:00:00.000Z","1974-05-01T00:00:00.000Z","1974-06-01T00:00:00.000Z","1974-07-01T00:00:00.000Z","1974-08-01T00:00:00.000Z","1974-09-01T00:00:00.000Z","1974-10-01T00:00:00.000Z","1974-11-01T00:00:00.000Z","1974-12-01T00:00:00.000Z","1975-01-01T00:00:00.000Z","1975-02-01T00:00:00.000Z","1975-03-01T00:00:00.000Z","1975-04-01T00:00:00.000Z","1975-05-01T00:00:00.000Z","1975-06-01T00:00:00.000Z","1975-07-01T00:00:00.000Z","1975-08-01T00:00:00.000Z","1975-09-01T00:00:00.000Z","1975-10-01T00:00:00.000Z","1975-11-01T00:00:00.000Z","1975-12-01T00:00:00.000Z","1976-01-01T00:00:00.000Z","1976-02-01T00:00:00.000Z","1976-03-01T00:00:00.000Z","1976-04-01T00:00:00.000Z","1976-05-01T00:00:00.000Z","1976-06-01T00:00:00.000Z","1976-07-01T00:00:00.000Z","1976-08-01T00:00:00.000Z","1976-09-01T00:00:00.000Z","1976-10-01T00:00:00.000Z","1976-11-01T00:00:00.000Z","1976-12-01T00:00:00.000Z","1977-01-01T00:00:00.000Z","1977-02-01T00:00:00.000Z","1977-03-01T00:00:00.000Z","1977-04-01T00:00:00.000Z","1977-05-01T00:00:00.000Z","1977-06-01T00:00:00.000Z","1977-07-01T00:00:00.000Z","1977-08-01T00:00:00.000Z","1977-09-01T00:00:00.000Z","1977-10-01T00:00:00.000Z","1977-11-01T00:00:00.000Z","1977-12-01T00:00:00.000Z","1978-01-01T00:00:00.000Z","1978-02-01T00:00:00.000Z","1978-03-01T00:00:00.000Z","1978-04-01T00:00:00.000Z","1978-05-01T00:00:00.000Z","1978-06-01T00:00:00.000Z","1978-07-01T00:00:00.000Z","1978-08-01T00:00:00.000Z","1978-09-01T00:00:00.000Z","1978-10-01T00:00:00.000Z","1978-11-01T00:00:00.000Z","1978-12-01T00:00:00.000Z","1979-01-01T00:00:00.000Z","1979-02-01T00:00:00.000Z","1979-03-01T00:00:00.000Z","1979-04-01T00:00:00.000Z","1979-05-01T00:00:00.000Z","1979-06-01T00:00:00.000Z","1979-07-01T00:00:00.000Z","1979-08-01T00:00:00.000Z","1979-09-01T00:00:00.000Z","1979-10-01T00:00:00.000Z","1979-11-01T00:00:00.000Z","1979-12-01T00:00:00.000Z"],[3035,2552,2704,2554,2014,1655,1721,1524,1596,2074,2199,2512,2933,2889,2938,2497,1870,1726,1607,1545,1396,1787,2076,2837,2787,3891,3179,2011,1636,1580,1489,1300,1356,1653,2013,2823,3102,2294,2385,2444,1748,1554,1498,1361,1346,1564,1640,2293,2815,3137,2679,1969,1870,1633,1529,1366,1357,1570,1535,2491,3084,2605,2573,2143,1693,1504,1461,1354,1333,1492,1781,1915]],"plugins":{"Crosshair":{"direction":"vertical"}}},"evals":["attrs.interactionModel"],"jsHooks":[]}
```
