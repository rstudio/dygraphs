# dygraph axis

Define options for an axis on a dygraph plot. Note that options will use
the default global setting (as determined by
[`dyOptions`](https://rstudio.github.io/dygraphs/reference/dyOptions.md))
when not specified explicitly.

## Usage

``` r
dyAxis(
  dygraph,
  name,
  label = NULL,
  valueRange = NULL,
  logscale = NULL,
  ticker = NULL,
  rangePad = NULL,
  labelWidth = NULL,
  labelHeight = NULL,
  axisHeight = NULL,
  axisLineColor = NULL,
  axisLineWidth = NULL,
  pixelsPerLabel = NULL,
  axisLabelColor = NULL,
  axisLabelFontSize = NULL,
  axisLabelWidth = NULL,
  axisLabelFormatter = NULL,
  valueFormatter = NULL,
  drawGrid = NULL,
  gridLineColor = NULL,
  gridLineWidth = NULL,
  independentTicks = NULL
)
```

## Arguments

- dygraph:

  Dygraph to add an axis definition to

- name:

  Axis name ('x', 'y', or 'y2')

- label:

  Label to display for axis (defaults to none).

- valueRange:

  Explicitly set the vertical range of the graph to `c(low, high)`. This
  may be set on a per-axis basis to define each y-axis separately. If
  either limit is unspecified, it will be calculated automatically (e.g.
  `c(NULL, 30)` to automatically calculate just the lower bound).

- logscale:

  When set for the y-axis or x-axis, the graph shows that axis in log
  scale. Any values less than or equal to zero are not displayed.
  Showing log scale with ranges that go below zero will result in an
  unviewable graph. Not compatible with showZero. connectSeparatedPoints
  is ignored. This is ignored for date-based x-axes.

- ticker:

  This lets you specify an arbitrary JavaScript function to generate
  tick marks on an axis. The tick marks are an array of (value, label)
  pairs. The built-in functions go to great lengths to choose good tick
  marks so, if you set this option, you'll most likely want to call one
  of them and modify the result. See dygraph-tickers.js and the
  [dygraphs documentation](https://dygraphs.com/options.html) for
  additional details).

- rangePad:

  Add the specified amount of extra space (in pixels) around the value
  range to ensure points at the edges remain visible.

- labelWidth:

  Width of the div which contains the y-axis label. Since the y-axis
  label appears rotated 90 degrees, this actually affects the height of
  its div.

- labelHeight:

  Height of the x-axis label, in pixels. This also controls the default
  font size of the x-axis label. If you style the label on your own,
  this controls how much space is set aside below the chart for the
  x-axis label's div.

- axisHeight:

  Height, in pixels, of the x-axis. If not set explicitly, this is
  computed based on `axisLabelFontSize` and `axisTickSize`.

- axisLineColor:

  Color of the x- and y-axis lines. Accepts any value which the HTML
  canvas strokeStyle attribute understands, e.g. 'black' or 'rgb(0, 100,
  255)'.

- axisLineWidth:

  Thickness (in pixels) of the x- and y-axis lines.

- pixelsPerLabel:

  Number of pixels to require between each x- and y-label. Larger values
  will yield a sparser axis with fewer ticks. Defaults to 50 (x-axis) or
  30 (y-axes).

- axisLabelColor:

  Color for x- and y-axis labels. This is a CSS color string. This may
  also be set globally using `dyOptions`.

- axisLabelFontSize:

  Size of the font (in pixels) to use in the axis labels, both x- and
  y-axis. This may also be set globally using `dyOptions`.

- axisLabelWidth:

  Width, in pixels, of the axis labels

- axisLabelFormatter:

  JavaScript function to call to format the tick values that appear
  along an axis (see the [dygraphs
  documentation](https://dygraphs.com/options.html) for additional
  details).

- valueFormatter:

  JavaScript function to call to provide a custom display format for the
  values displayed on mouseover (see the [dygraphs
  documentation](https://dygraphs.com/options.html) for additional
  details).

- drawGrid:

  Whether to display grid lines in the chart.

- gridLineColor:

  The color of the grid lines.

- gridLineWidth:

  Thickness (in pixels) of the grid lines drawn under the chart.

- independentTicks:

  Only valid for y and y2, has no effect on x: This option defines
  whether the y axes should align their ticks or if they should be
  independent. Possible combinations: 1.) y=true, y2=false (default): y
  is the primary axis and the y2 ticks are aligned to the the ones of y.
  (only 1 grid) 2.) y=false, y2=true: y2 is the primary axis and the y
  ticks are aligned to the the ones of y2. (only 1 grid) 3.) y=true,
  y2=true: Both axis are independent and have their own ticks. (2 grids)
  4.) y=false, y2=false: Invalid configuration causes an error.

## Value

Axis options

## Note

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-axis-options.html)
for additional details and examples.

## Examples

``` r
library(dygraphs)

dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)

{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60,"drawAxis":true},"y":{"valueRange":[40,60],"drawAxis":true}},"title":"New Haven Temperatures","labels":["year","V1"],"legend":"auto","retainDateWindow":false,"ylabel":"Temp (F)","stackedGraph":false,"fillGraph":true,"fillAlpha":0.15,"stepPlot":false,"drawPoints":false,"pointSize":1,"drawGapEdgePoints":false,"connectSeparatedPoints":false,"strokeWidth":1,"strokeBorderColor":"white","colorValue":0.5,"colorSaturation":1,"includeZero":false,"drawAxesAtZero":false,"logscale":false,"axisTickSize":3,"axisLineColor":"black","axisLineWidth":1.5,"axisLabelColor":"black","axisLabelFontSize":14,"axisLabelWidth":60,"drawGrid":false,"gridLineWidth":0.3,"rightGap":5,"digitsAfterDecimal":2,"labelsKMB":false,"labelsKMG2":false,"labelsUTC":false,"maxNumberWidth":6,"animatedZooms":false,"mobileDisableYTouch":true,"disableZoom":false},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]],"fixedtz":false,"tzone":"UTC"},"evals":[],"jsHooks":[]}
```
