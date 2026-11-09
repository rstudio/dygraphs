# Package index

## Getting Started

Create interactive time series visualizations with dygraphs.

- [`dygraph()`](https://rstudio.github.io/dygraphs/reference/dygraph.md)
  : dygraph interactive plot for time series data
- [`dygraphs-exports`](https://rstudio.github.io/dygraphs/reference/dygraphs-exports.md)
  [`%>%`](https://rstudio.github.io/dygraphs/reference/dygraphs-exports.md)
  [`as.yearmon`](https://rstudio.github.io/dygraphs/reference/dygraphs-exports.md)
  [`as.yearqtr`](https://rstudio.github.io/dygraphs/reference/dygraphs-exports.md)
  : dygraph exported operators and S3 methods

## Axes, Series and Display

Control axes, labels, legends and series appearance.

- [`dyAxis()`](https://rstudio.github.io/dygraphs/reference/dyAxis.md) :
  dygraph axis
- [`dySeries()`](https://rstudio.github.io/dygraphs/reference/dySeries.md)
  : dygraph data series
- [`dyGroup()`](https://rstudio.github.io/dygraphs/reference/dyGroup.md)
  : dygraph series group
- [`dyLegend()`](https://rstudio.github.io/dygraphs/reference/dyLegend.md)
  : dygraph legend
- [`dyOptions()`](https://rstudio.github.io/dygraphs/reference/dyOptions.md)
  : dygraph options
- [`dyCSS()`](https://rstudio.github.io/dygraphs/reference/dyCSS.md) :
  CSS for dygraph labels and legend

## Interactivity

Add zooming, highlighting, crosshairs and other interactive features.

- [`dyRangeSelector()`](https://rstudio.github.io/dygraphs/reference/dyRangeSelector.md)
  : dygraph interactive range selection and zooming
- [`dyHighlight()`](https://rstudio.github.io/dygraphs/reference/dyHighlight.md)
  : dygraph series mouse-over highlighting
- [`dyCrosshair()`](https://rstudio.github.io/dygraphs/reference/dyCrosshair.md)
  : The dyCrosshair plugin draws a crosshair line over the point closest
  to the mouse when the user hovers over the graph. It has a "direction"
  option which is provided in the R wrapper function and then forwarded
  to the plugin using the "options" argument to dyPlugin.
- [`dyRoller()`](https://rstudio.github.io/dygraphs/reference/dyRoller.md)
  : dygraph rolling average period text box
- [`dyUnzoom()`](https://rstudio.github.io/dygraphs/reference/dyUnzoom.md)
  : The dyUnzoom plugin adds an "Unzoom" button to the graph when it's
  displaying in a zoomed state (this is a bit more discoverable than the
  default double- click gesture for unzooming).

## Annotations and Overlays

Add contextual information to charts.

- [`dyAnnotation()`](https://rstudio.github.io/dygraphs/reference/dyAnnotation.md)
  : Annotation for dygraph chart
- [`dyEvent()`](https://rstudio.github.io/dygraphs/reference/dyEvent.md)
  : dygraph event line
- [`dyLimit()`](https://rstudio.github.io/dygraphs/reference/dyLimit.md)
  : dygraph limit line
- [`dyShading()`](https://rstudio.github.io/dygraphs/reference/dyShading.md)
  : dygraph region shading
- [`dyRibbon()`](https://rstudio.github.io/dygraphs/reference/dyRibbon.md)
  : dyRibbon plugin adds a horizontal band of colors that runs through
  the chart. It can be useful to visualize categorical variables
  (http://en.wikipedia.org/wiki/Categorical_variable) that change over
  time (along the x-axis).

## Plot Types and Plotters

Alternative visual representations and custom plotters.

- [`dyPlotter()`](https://rstudio.github.io/dygraphs/reference/dyPlotter.md)
  : Include a dygraph plotter
- [`dyCandlestick()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyBarChart()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyStackedBarChart()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyMultiColumn()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyBarSeries()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyStemSeries()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyShadow()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyFilledLine()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyErrorFill()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyMultiColumnGroup()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyCandlestickGroup()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyStackedBarGroup()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyStackedLineGroup()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  [`dyStackedRibbonGroup()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
  : Employ a dygraph plotter on a series, a group of series, or the
  whole dygraph

## Plugins and Extensions

Extend dygraphs with plugins, callbacks and dependencies.

- [`dyPlugin()`](https://rstudio.github.io/dygraphs/reference/dyPlugin.md)
  : Include a dygraph plugin
- [`dyCallbacks()`](https://rstudio.github.io/dygraphs/reference/dyCallbacks.md)
  : Callbacks for dygraph events
- [`dyDependency()`](https://rstudio.github.io/dygraphs/reference/dyDependency.md)
  : Add external assets as a dygraph dependency

## Data and Transformations

Utilities for supplying and transforming data.

- [`dySeriesData()`](https://rstudio.github.io/dygraphs/reference/dySeriesData.md)
  : Add series data to dygraph
- [`dyDataHandler()`](https://rstudio.github.io/dygraphs/reference/dyDataHandler.md)
  : Include a dygraph data handler
- [`dyRebase()`](https://rstudio.github.io/dygraphs/reference/dyRebase.md)
  : Rebase data handler for straw broom charts with Dygraph

## Shiny Integration

Use dygraphs within Shiny applications.

- [`dygraphOutput()`](https://rstudio.github.io/dygraphs/reference/dygraph-shiny.md)
  [`renderDygraph()`](https://rstudio.github.io/dygraphs/reference/dygraph-shiny.md)
  : Shiny bindings for dygraph
