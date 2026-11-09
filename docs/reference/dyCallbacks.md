# Callbacks for dygraph events

Set JavaScript callbacks for various dygraph events. See the [dygraph
options](https://dygraphs.com/options.html) reference for additional
details on the signature of each callback.

## Usage

``` r
dyCallbacks(
  dygraph,
  clickCallback = NULL,
  drawCallback = NULL,
  highlightCallback = NULL,
  pointClickCallback = NULL,
  underlayCallback = NULL,
  unhighlightCallback = NULL,
  zoomCallback = NULL,
  drawHighlightPointCallback = NULL,
  drawPointCallback = NULL,
  annotationClickHandler = NULL,
  annotationMouseOverHandler = NULL,
  annotationMouseOutHandler = NULL,
  annotationDblClickHandler = NULL
)
```

## Arguments

- dygraph:

  Dygraph to add callbacks to

- clickCallback:

  A function to call when the canvas is clicked.

- drawCallback:

  When set, this callback gets called every time the dygraph is drawn.
  This includes the initial draw, after zooming and repeatedly while
  panning.

- highlightCallback:

  When set, this callback gets called every time a new point is
  highlighted.

- pointClickCallback:

  A function to call when a data point is clicked. and the point that
  was clicked.

- underlayCallback:

  When set, this callback gets called before the chart is drawn.

- unhighlightCallback:

  When set, this callback gets called every time the user stops
  highlighting any point by mousing out of the graph.

- zoomCallback:

  A function to call when the zoom window is changed (either by zooming
  in or out).

- drawHighlightPointCallback:

  Draw a custom item when a point is highlighted. Default is a small dot
  matching the series color. This method should constrain drawing to
  within pointSize pixels from (cx, cy)

- drawPointCallback:

  Draw a custom item when drawPoints is enabled. Default is a small dot
  matching the series color. This method should constrain drawing to
  within pointSize pixels from (cx, cy).

- annotationClickHandler:

  JavaScript function to call when an annotation is clicked. This can
  also be specified on a per-annotation basis.

- annotationMouseOverHandler:

  JavaScript function to call when the mouse hovers over an annotation.
  This can also be specified on a per-annotation basis.

- annotationMouseOutHandler:

  JavaScript function to call when the mouse exits an annotation. This
  can also be specified on a per-annotation basis.

- annotationDblClickHandler:

  JavaScript function to call when an annotation is double clicked. This
  can also be specified on a per-annotation basis.

## Value

Dygraph with callbacks
