# dygraph data series

Add a data series to a dygraph plot. Note that options will use the
default global setting (as determined by
[`dyOptions`](https://rstudio.github.io/dygraphs/reference/dyOptions.md))
when not specified explicitly. When no `dySeries` is specified for a
plot then all series within the underlying data are plotted.

## Usage

``` r
dySeries(
  dygraph,
  name = NULL,
  label = NULL,
  color = NULL,
  axis = "y",
  stepPlot = NULL,
  stemPlot = NULL,
  fillGraph = NULL,
  drawPoints = NULL,
  pointSize = NULL,
  pointShape = NULL,
  strokeWidth = NULL,
  strokePattern = NULL,
  strokeBorderWidth = NULL,
  strokeBorderColor = NULL,
  plotter = NULL
)
```

## Arguments

- dygraph:

  Dygraph to add a series definition to

- name:

  Name of series within data set. If no name is specified then series
  are bound to implicitly based on their order within the underlying
  time series object. This parameter can also be a character vector of
  length 3 that specifies a set of input column names to use as the
  lower, value, and upper for a series with a shaded bar drawn around
  it.

- label:

  Label to display for series (uses name if no label defined)

- color:

  Color for series. These can be of the form "#AABBCC" or
  "rgb(255,100,200)" or "yellow", etc. If not specified then the global
  colors option (typically based on equally-spaced points around a color
  wheel).

- axis:

  Y-axis to associate the series with ("y" or "y2")

- stepPlot:

  When set, display the graph as a step plot instead of a line plot.

- stemPlot:

  When set, display the graph as a stem plot instead of a line plot.

- fillGraph:

  Should the area underneath the graph be filled? This option is not
  compatible with error bars.

- drawPoints:

  Draw a small dot at each point, in addition to a line going through
  the point. This makes the individual data points easier to see, but
  can increase visual clutter in the chart.

- pointSize:

  The size of the dot to draw on each point in pixels. A dot is always
  drawn when a point is "isolated", i.e. there is a missing point on
  either side of it. This also controls the size of those dots.

- pointShape:

  The shape of the dot to draw. Can be one of the following: "dot"
  (default), "triangle", "square", "diamond", "pentagon", "hexagon",
  "circle", "star", "plus" or "ex".

- strokeWidth:

  The width of the lines connecting data points. This can be used to
  increase the contrast or some graphs.

- strokePattern:

  A predefined stroke pattern type ("dotted", "dashed", or "dotdash") or
  a custom pattern array where the even index is a draw and odd is a
  space in pixels. If `NULL` then it draws a solid line. The array
  should have an even length as any odd length array could be expressed
  as a smaller even length array.

- strokeBorderWidth:

  Draw a border around graph lines to make crossing lines more easily
  distinguishable. Useful for graphs with many lines.

- strokeBorderColor:

  Color for the line border used if `strokeBorderWidth` is set.

- plotter:

  A function which plots the data series. May also be set on on a global
  basis using `dyOptions`. See the [dygraphs
  documentation](https://dygraphs.com/tests/plotters.html) for
  additional details on plotting functions.

## Value

Dygraph with additional series

## Note

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-series-options.html)
for additional details and examples.

## Examples

``` r
library(dygraphs)

lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)

dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dySeries("mdeaths", drawPoints = TRUE, color = "blue") %>%
  dySeries("fdeaths", stepPlot = TRUE, color = "red")

{"x":{"attrs":{"title":"Deaths from Lung Disease (UK)","labels":["month","ldeaths","mdeaths","fdeaths"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"colors":["#408000","blue","red"],"series":{"mdeaths":{"axis":"y","drawPoints":true},"fdeaths":{"axis":"y","stepPlot":true}}},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z"],[3035,2552,2704,2554,2014,1655,1721,1524,1596,2074,2199,2512,2933,2889,2938,2497,1870,1726,1607,1545,1396,1787,2076,2837,2787,3891,3179,2011,1636,1580,1489,1300,1356,1653,2013,2823,3102,2294,2385,2444,1748,1554,1498,1361,1346,1564,1640,2293,2815,3137,2679,1969,1870,1633,1529,1366,1357,1570,1535,2491,3084,2605,2573,2143,1693,1504,1461,1354,1333,1492,1781,1915],[2134,1863,1877,1877,1492,1249,1280,1131,1209,1492,1621,1846,2103,2137,2153,1833,1403,1288,1186,1133,1053,1347,1545,2066,2020,2750,2283,1479,1189,1160,1113,970,999,1208,1467,2059,2240,1634,1722,1801,1246,1162,1087,1013,959,1179,1229,1655,2019,2284,1942,1423,1340,1187,1098,1004,970,1140,1110,1812,2263,1820,1846,1531,1215,1075,1056,975,940,1081,1294,1341],[901,689,827,677,522,406,441,393,387,582,578,666,830,752,785,664,467,438,421,412,343,440,531,771,767,1141,896,532,447,420,376,330,357,445,546,764,862,660,663,643,502,392,411,348,387,385,411,638,796,853,737,546,530,446,431,362,387,430,425,679,821,785,727,612,478,429,405,379,393,411,487,574]]},"evals":[],"jsHooks":[]}
```
