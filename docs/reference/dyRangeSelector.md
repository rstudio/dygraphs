# dygraph interactive range selection and zooming

Add a range selector to the bottom of the chart that allows users to pan
and zoom to various date ranges.

## Usage

``` r
dyRangeSelector(
  dygraph,
  dateWindow = NULL,
  height = 40,
  fillColor = " #A7B1C4",
  strokeColor = "#808FAB",
  keepMouseZoom = TRUE,
  retainDateWindow = FALSE
)
```

## Arguments

- dygraph:

  Dygraph to add range selector to

- dateWindow:

  Initially zoom in on a section of the graph. Is a two element vector
  \[earliest, latest\], where earliest/latest objects convertible via
  `as.POSIXct`.

- height:

  Height, in pixels, of the range selector widget.

- fillColor:

  The range selector mini plot fill color. This can be of the form
  "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify ""
  to turn off fill.

- strokeColor:

  The range selector mini plot stroke color. This can be of the form
  "#AABBCC" or "rgb(255,100,200)" or "yellow". You can also specify ""
  to turn off stroke.

- keepMouseZoom:

  Keep mouse zoom when adding a range selector

- retainDateWindow:

  Whether to retain the user's current date window (zoom level) when
  updating an existing dygraph with new data and/or options.

## Value

A dygraph that displays a range selector

## Note

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-range-selector.html)
for additional details and examples.

Shiny applications can respond to changes in the `dateWindow` via a
special date window shiny input value. For example, if the output id of
a dygraph is \`series\` then the current date window can be read from
`input$series_date_window` as an array of two date values (from and to).

## Examples

``` r
library(dygraphs)

dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector()

{"x":{"attrs":{"title":"New Haven Temperatures","labels":["year","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]]},"evals":["attrs.interactionModel"],"jsHooks":[]}
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector(dateWindow = c("1920-01-01", "1960-01-01"))

{"x":{"attrs":{"title":"New Haven Temperatures","labels":["year","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"dateWindow":["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],"rangeSelectorHeight":40,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"#808FAB","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]]},"evals":["attrs.interactionModel"],"jsHooks":[]}
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyRangeSelector(height = 20, strokeColor = "")

{"x":{"attrs":{"title":"New Haven Temperatures","labels":["year","V1"],"legend":"auto","retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"showRangeSelector":true,"rangeSelectorHeight":20,"rangeSelectorPlotFillColor":" #A7B1C4","rangeSelectorPlotStrokeColor":"","interactionModel":"Dygraph.Interaction.defaultModel"},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z","04Y-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]]},"evals":["attrs.interactionModel"],"jsHooks":[]}
```
