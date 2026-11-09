# dygraph interactive plot for time series data

R interface to interactive time series plotting using the
[dygraphs](https://dygraphs.com) JavaScript library.

## Usage

``` r
dygraph(
  data,
  main = NULL,
  xlab = NULL,
  ylab = NULL,
  periodicity = NULL,
  group = NULL,
  elementId = NULL,
  width = NULL,
  height = NULL
)
```

## Arguments

- data:

  Either time series data or numeric data. For time series, this must be
  an [xts](https://rdrr.io/pkg/xts/man/xts.html) object or an object
  which is convertible to `xts`. For numeric data, this must be a named
  list or data frame, where the first element/column provides x-axis
  values and all subsequent elements/columns provide one or more series
  of y-values.

- main:

  Main plot title (optional)

- xlab:

  X axis label

- ylab:

  Y axis label

- periodicity:

  Periodicity of time series data (automatically detected via
  [xts::periodicity](https://rdrr.io/pkg/xts/man/periodicity.html) if
  not specified).

- group:

  Group to associate this plot with. The x-axis zoom level of plots
  within a group is automatically synchronized.

- elementId:

  Use an explicit element ID for the widget (rather than an
  automatically generated one). Useful if you have other JavaScript that
  needs to explicitly discover and interact with a specific widget
  instance.

- width:

  Width in pixels (optional, defaults to automatic sizing)

- height:

  Height in pixels (optional, defaults to automatic sizing)

## Value

Interactive dygraph plot

## Note

See the [online documentation](https://rstudio.github.io/dygraphs/) for
additional details and examples.

## Examples

``` r
library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

{"x":{"attrs":{"labels":["month","mdeaths","fdeaths"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"scale":"monthly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-02-01T00:00:00.000Z","04Y-03-01T00:00:00.000Z","04Y-04-01T00:00:00.000Z","04Y-05-01T00:00:00.000Z","04Y-06-01T00:00:00.000Z","04Y-07-01T00:00:00.000Z","04Y-08-01T00:00:00.000Z","04Y-09-01T00:00:00.000Z","04Y-10-01T00:00:00.000Z","04Y-11-01T00:00:00.000Z","04Y-12-01T00:00:00.000Z"],[2134,1863,1877,1877,1492,1249,1280,1131,1209,1492,1621,1846,2103,2137,2153,1833,1403,1288,1186,1133,1053,1347,1545,2066,2020,2750,2283,1479,1189,1160,1113,970,999,1208,1467,2059,2240,1634,1722,1801,1246,1162,1087,1013,959,1179,1229,1655,2019,2284,1942,1423,1340,1187,1098,1004,970,1140,1110,1812,2263,1820,1846,1531,1215,1075,1056,975,940,1081,1294,1341],[901,689,827,677,522,406,441,393,387,582,578,666,830,752,785,664,467,438,421,412,343,440,531,771,767,1141,896,532,447,420,376,330,357,445,546,764,862,660,663,643,502,392,411,348,387,385,411,638,796,853,737,546,530,446,431,362,387,430,425,679,821,785,727,612,478,429,405,379,393,411,487,574]]},"evals":[],"jsHooks":[]}
indoConc <- Indometh[Indometh$Subject == 1, c("time", "conc")]
dygraph(indoConc)

{"x":{"attrs":{"labels":["time","conc"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}}},"annotations":[],"shadings":[],"events":[],"format":"numeric","data":[[0.25,0.5,0.75,1,1.25,2,3,4,5,6,8],[1.5,0.9399999999999999,0.78,0.48,0.37,0.19,0.12,0.11,0.08,0.07000000000000001,0.05]]},"evals":[],"jsHooks":[]}
```
