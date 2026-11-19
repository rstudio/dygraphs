# Annotation for dygraph chart

Define a text annotation for a data-point on a dygraph chart.

## Usage

``` r
dyAnnotation(
  dygraph,
  x,
  text,
  tooltip = NULL,
  width = NULL,
  height = NULL,
  cssClass = NULL,
  tickHeight = NULL,
  attachAtBottom = FALSE,
  clickHandler = NULL,
  mouseOverHandler = NULL,
  mouseOutHandler = NULL,
  dblClickHandler = NULL,
  series = NULL
)
```

## Arguments

- dygraph:

  Dygraph to add an annotation to

- x:

  Either numeric or date value indicating where to place the annotation.
  For date value, this should be of class `POSIXct` or convertible to
  `POSIXct`.

- text:

  Text to overlay on the chart at the location of x

- tooltip:

  Additional tooltip text to display on mouse hover

- width:

  Width (in pixels) of the annotation flag.

- height:

  Height (in pixels) of the annotation flag.

- cssClass:

  CSS class to use for styling the annotation.

- tickHeight:

  Height of the tick mark (in pixels) connecting the point to its flag
  or icon.

- attachAtBottom:

  If true, attach annotations to the x-axis, rather than to actual
  points.

- clickHandler:

  JavaScript function to call when an annotation is clicked.

- mouseOverHandler:

  JavaScript function to call when the mouse hovers over an annotation.

- mouseOutHandler:

  JavaScript function to call when the mouse exits an annotation.

- dblClickHandler:

  JavaScript function to call when an annotation is double clicked.

- series:

  Series to attach the annotation to. By default, the last series
  defined using
  [`dySeries`](https://rstudio.github.io/dygraphs/reference/dySeries.md).

## Value

Dygraph with specified annotation

## Note

Annotations are bound to specific series in the input data. If you have
only one series or if you define annotations immediately after a call to
[`dySeries`](https://rstudio.github.io/dygraphs/reference/dySeries.md)
then you need not specify the series explicitly. Otherwise, you should
use the `series` parameter to indicate which series the annotation
should be bound to.

Annotation event handlers can also specified globally (see
[`dyCallbacks`](https://rstudio.github.io/dygraphs/reference/dyCallbacks.md)).

See the [online
documentation](https://rstudio.github.io/dygraphs/gallery-annotations.html)
for additional details and examples.

## Examples

``` r
library(dygraphs)

dygraph(presidents, main = "Presidential Approval") %>%
  dyAxis("y", valueRange = c(0, 100)) %>%
  dyAnnotation("1950-7-1", text = "A", tooltip = "Korea") %>%
  dyAnnotation("1965-1-1", text = "B", tooltip = "Vietnam")

{"x":{"attrs":{"axes":{"x":{"pixelsPerLabel":60},"y":{"valueRange":[0,100]}},"title":"Presidential Approval","labels":["quarter","V1"],"legend":"auto","retainDateWindow":false},"scale":"quarterly","annotations":[{"x":"1950-07-01T00:00:00.000Z","shortText":"A","text":"Korea","attachAtBottom":false,"series":"V1"},{"x":"1965-01-01T00:00:00.000Z","shortText":"B","text":"Vietnam","attachAtBottom":false,"series":"V1"}],"shadings":[],"events":[],"format":"date","data":[["1945-01-01T00:00:00.000Z","1945-04-01T00:00:00.000Z","1945-07-01T00:00:00.000Z","1945-10-01T00:00:00.000Z","1946-01-01T00:00:00.000Z","1946-04-01T00:00:00.000Z","1946-07-01T00:00:00.000Z","1946-10-01T00:00:00.000Z","1947-01-01T00:00:00.000Z","1947-04-01T00:00:00.000Z","1947-07-01T00:00:00.000Z","1947-10-01T00:00:00.000Z","1948-01-01T00:00:00.000Z","1948-04-01T00:00:00.000Z","1948-07-01T00:00:00.000Z","1948-10-01T00:00:00.000Z","1949-01-01T00:00:00.000Z","1949-04-01T00:00:00.000Z","1949-07-01T00:00:00.000Z","1949-10-01T00:00:00.000Z","1950-01-01T00:00:00.000Z","1950-04-01T00:00:00.000Z","1950-07-01T00:00:00.000Z","1950-10-01T00:00:00.000Z","1951-01-01T00:00:00.000Z","1951-04-01T00:00:00.000Z","1951-07-01T00:00:00.000Z","1951-10-01T00:00:00.000Z","1952-01-01T00:00:00.000Z","1952-04-01T00:00:00.000Z","1952-07-01T00:00:00.000Z","1952-10-01T00:00:00.000Z","1953-01-01T00:00:00.000Z","1953-04-01T00:00:00.000Z","1953-07-01T00:00:00.000Z","1953-10-01T00:00:00.000Z","1954-01-01T00:00:00.000Z","1954-04-01T00:00:00.000Z","1954-07-01T00:00:00.000Z","1954-10-01T00:00:00.000Z","1955-01-01T00:00:00.000Z","1955-04-01T00:00:00.000Z","1955-07-01T00:00:00.000Z","1955-10-01T00:00:00.000Z","1956-01-01T00:00:00.000Z","1956-04-01T00:00:00.000Z","1956-07-01T00:00:00.000Z","1956-10-01T00:00:00.000Z","1957-01-01T00:00:00.000Z","1957-04-01T00:00:00.000Z","1957-07-01T00:00:00.000Z","1957-10-01T00:00:00.000Z","1958-01-01T00:00:00.000Z","1958-04-01T00:00:00.000Z","1958-07-01T00:00:00.000Z","1958-10-01T00:00:00.000Z","1959-01-01T00:00:00.000Z","1959-04-01T00:00:00.000Z","1959-07-01T00:00:00.000Z","1959-10-01T00:00:00.000Z","1960-01-01T00:00:00.000Z","1960-04-01T00:00:00.000Z","1960-07-01T00:00:00.000Z","1960-10-01T00:00:00.000Z","1961-01-01T00:00:00.000Z","1961-04-01T00:00:00.000Z","1961-07-01T00:00:00.000Z","1961-10-01T00:00:00.000Z","1962-01-01T00:00:00.000Z","1962-04-01T00:00:00.000Z","1962-07-01T00:00:00.000Z","1962-10-01T00:00:00.000Z","1963-01-01T00:00:00.000Z","1963-04-01T00:00:00.000Z","1963-07-01T00:00:00.000Z","1963-10-01T00:00:00.000Z","1964-01-01T00:00:00.000Z","1964-04-01T00:00:00.000Z","1964-07-01T00:00:00.000Z","1964-10-01T00:00:00.000Z","1965-01-01T00:00:00.000Z","1965-04-01T00:00:00.000Z","1965-07-01T00:00:00.000Z","1965-10-01T00:00:00.000Z","1966-01-01T00:00:00.000Z","1966-04-01T00:00:00.000Z","1966-07-01T00:00:00.000Z","1966-10-01T00:00:00.000Z","1967-01-01T00:00:00.000Z","1967-04-01T00:00:00.000Z","1967-07-01T00:00:00.000Z","1967-10-01T00:00:00.000Z","1968-01-01T00:00:00.000Z","1968-04-01T00:00:00.000Z","1968-07-01T00:00:00.000Z","1968-10-01T00:00:00.000Z","1969-01-01T00:00:00.000Z","1969-04-01T00:00:00.000Z","1969-07-01T00:00:00.000Z","1969-10-01T00:00:00.000Z","1970-01-01T00:00:00.000Z","1970-04-01T00:00:00.000Z","1970-07-01T00:00:00.000Z","1970-10-01T00:00:00.000Z","1971-01-01T00:00:00.000Z","1971-04-01T00:00:00.000Z","1971-07-01T00:00:00.000Z","1971-10-01T00:00:00.000Z","1972-01-01T00:00:00.000Z","1972-04-01T00:00:00.000Z","1972-07-01T00:00:00.000Z","1972-10-01T00:00:00.000Z","1973-01-01T00:00:00.000Z","1973-04-01T00:00:00.000Z","1973-07-01T00:00:00.000Z","1973-10-01T00:00:00.000Z","1974-01-01T00:00:00.000Z","1974-04-01T00:00:00.000Z","1974-07-01T00:00:00.000Z","1974-10-01T00:00:00.000Z"],[null,87,82,75,63,50,43,32,35,60,54,55,36,39,null,null,69,57,57,51,45,37,46,39,36,24,32,23,25,32,null,32,59,74,75,60,71,61,71,57,71,68,79,73,76,71,67,75,79,62,63,57,60,49,48,52,57,62,61,66,71,62,61,57,72,83,71,78,79,71,62,74,76,64,62,57,80,73,69,69,71,64,69,62,63,46,56,44,44,52,38,46,36,49,35,44,59,65,65,56,66,53,61,52,51,48,54,49,49,61,null,null,68,44,40,27,28,25,24,24]]},"evals":[],"jsHooks":[]}
```
