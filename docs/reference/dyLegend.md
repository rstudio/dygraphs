# dygraph legend

Configure options for the dygraph series legend.

## Usage

``` r
dyLegend(
  dygraph,
  show = c("auto", "always", "onmouseover", "follow", "never"),
  width = 250,
  showZeroValues = TRUE,
  labelsDiv = NULL,
  labelsSeparateLines = FALSE,
  hideOnMouseOut = TRUE
)
```

## Arguments

- dygraph:

  Dygraph to configure legend options for.

- show:

  When to display the legend. Specify "always" to always show the
  legend. Specify "onmouseover" to only display it when a user mouses
  over the chart. Specify "follow" to have the legend show as overlay to
  the chart which follows the mouse. The default behavior is "auto",
  which results in "always" when more than one series is plotted and
  "onmouseover" when only a single series is plotted.

- width:

  Width (in pixels) of the div which shows the legend.

- showZeroValues:

  Show zero value labels in the legend.

- labelsDiv:

  Show data labels in an external div, rather than on the graph. This
  value should be a div element id.

- labelsSeparateLines:

  Put a \<br/\> between lines in the label string. Often used in
  conjunction with `labelsDiv`.

- hideOnMouseOut:

  Whether to hide the legend when the mouse leaves the chart area. This
  option applies when `show` is set to "onmouseover". Note that this
  also affects the hiding of the
  [`dyHighlight`](https://rstudio.github.io/dygraphs/reference/dyHighlight.md)
  on mouse out.

## Value

A dygraph with customized legend options

## Note

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-plot-labels.html)
for additional details and examples.

## Examples

``` r
library(dygraphs)

dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dySeries("V1", label = "Temperature (F)") %>%
  dyLegend(show = "always", hideOnMouseOut = FALSE)

{"x":{"attrs":{"title":"New Haven Temperatures","labels":["year","Temperature (F)"],"retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"series":{"Temperature (F)":{"axis":"y"}},"legend":"always","labelsDivWidth":250,"labelsShowZeroValues":true,"labelsSeparateLines":false,"hideOverlayOnMouseOut":false},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]]},"evals":[],"jsHooks":[]}
```
