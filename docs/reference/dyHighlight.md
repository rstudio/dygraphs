# dygraph series mouse-over highlighting

Configure options for data series mouse-over highlighting. Note that
highlighting is always enabled for dygraphs so this function is used to
customize rather than enable highlighting.

## Usage

``` r
dyHighlight(
  dygraph,
  highlightCircleSize = 3,
  highlightSeriesBackgroundAlpha = 0.5,
  highlightSeriesOpts = list(),
  hideOnMouseOut = TRUE
)
```

## Arguments

- dygraph:

  Dygraph to configure highlighting behavior for.

- highlightCircleSize:

  The size in pixels of the dot drawn over highlighted points.

- highlightSeriesBackgroundAlpha:

  Fade the background while highlighting series. 1=fully visible
  background (disable fading), 0=hidden background (show highlighted
  series only).

- highlightSeriesOpts:

  When set, the options from this list are applied to the series closest
  to the mouse pointer for interactive highlighting. Example:
  list(strokeWidth = 3). See the documentation on
  [`dySeries`](https://rstudio.github.io/dygraphs/reference/dySeries.md)
  for additional details on options that can be set.

- hideOnMouseOut:

  Whether to hide the highlighting effects when the mouse leaves the
  chart area. Note that this also affects the hiding of the
  [`dyLegend`](https://rstudio.github.io/dygraphs/reference/dyLegend.md)
  on mouse out.

## Value

A dygraph with customized highlighting options

## Note

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-series-highlighting.html)
for additional details and examples.

## Examples

``` r
library(dygraphs)
lungDeaths <- cbind(ldeaths, mdeaths, fdeaths)
dygraph(lungDeaths, main = "Deaths from Lung Disease (UK)") %>%
  dyHighlight(
    highlightCircleSize = 5,
    highlightSeriesBackgroundAlpha = 0.2,
    hideOnMouseOut = FALSE
  )

{"x":{"attrs":{"title":"Deaths from Lung Disease (UK)","labels":["month","ldeaths","mdeaths","fdeaths"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"highlightCircleSize":5,"highlightSeriesBackgroundAlpha":0.2,"highlightSeriesOpts":[],"hideOverlayOnMouseOut":false},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z"],[3035,2552,2704,2554,2014,1655,1721,1524,1596,2074,2199,2512,2933,2889,2938,2497,1870,1726,1607,1545,1396,1787,2076,2837,2787,3891,3179,2011,1636,1580,1489,1300,1356,1653,2013,2823,3102,2294,2385,2444,1748,1554,1498,1361,1346,1564,1640,2293,2815,3137,2679,1969,1870,1633,1529,1366,1357,1570,1535,2491,3084,2605,2573,2143,1693,1504,1461,1354,1333,1492,1781,1915],[2134,1863,1877,1877,1492,1249,1280,1131,1209,1492,1621,1846,2103,2137,2153,1833,1403,1288,1186,1133,1053,1347,1545,2066,2020,2750,2283,1479,1189,1160,1113,970,999,1208,1467,2059,2240,1634,1722,1801,1246,1162,1087,1013,959,1179,1229,1655,2019,2284,1942,1423,1340,1187,1098,1004,970,1140,1110,1812,2263,1820,1846,1531,1215,1075,1056,975,940,1081,1294,1341],[901,689,827,677,522,406,441,393,387,582,578,666,830,752,785,664,467,438,421,412,343,440,531,771,767,1141,896,532,447,420,376,330,357,445,546,764,862,660,663,643,502,392,411,348,387,385,411,638,796,853,737,546,530,446,431,362,387,430,425,679,821,785,727,612,478,429,405,379,393,411,487,574]]},"evals":[],"jsHooks":[]}
```
