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

{"x":{"attrs":{"title":"New Haven Temperatures","labels":["year","Temperature (F)"],"retainDateWindow":false,"axes":{"x":{"pixelsPerLabel":60}},"series":{"Temperature (F)":{"axis":"y"}},"legend":"always","labelsDivWidth":250,"labelsShowZeroValues":true,"labelsSeparateLines":false,"hideOverlayOnMouseOut":false},"scale":"yearly","annotations":[],"shadings":[],"events":[],"format":"date","data":[["1912-01-01T00:00:00.000Z","1913-01-01T00:00:00.000Z","1914-01-01T00:00:00.000Z","1915-01-01T00:00:00.000Z","1916-01-01T00:00:00.000Z","1917-01-01T00:00:00.000Z","1918-01-01T00:00:00.000Z","1919-01-01T00:00:00.000Z","1920-01-01T00:00:00.000Z","1921-01-01T00:00:00.000Z","1922-01-01T00:00:00.000Z","1923-01-01T00:00:00.000Z","1924-01-01T00:00:00.000Z","1925-01-01T00:00:00.000Z","1926-01-01T00:00:00.000Z","1927-01-01T00:00:00.000Z","1928-01-01T00:00:00.000Z","1929-01-01T00:00:00.000Z","1930-01-01T00:00:00.000Z","1931-01-01T00:00:00.000Z","1932-01-01T00:00:00.000Z","1933-01-01T00:00:00.000Z","1934-01-01T00:00:00.000Z","1935-01-01T00:00:00.000Z","1936-01-01T00:00:00.000Z","1937-01-01T00:00:00.000Z","1938-01-01T00:00:00.000Z","1939-01-01T00:00:00.000Z","1940-01-01T00:00:00.000Z","1941-01-01T00:00:00.000Z","1942-01-01T00:00:00.000Z","1943-01-01T00:00:00.000Z","1944-01-01T00:00:00.000Z","1945-01-01T00:00:00.000Z","1946-01-01T00:00:00.000Z","1947-01-01T00:00:00.000Z","1948-01-01T00:00:00.000Z","1949-01-01T00:00:00.000Z","1950-01-01T00:00:00.000Z","1951-01-01T00:00:00.000Z","1952-01-01T00:00:00.000Z","1953-01-01T00:00:00.000Z","1954-01-01T00:00:00.000Z","1955-01-01T00:00:00.000Z","1956-01-01T00:00:00.000Z","1957-01-01T00:00:00.000Z","1958-01-01T00:00:00.000Z","1959-01-01T00:00:00.000Z","1960-01-01T00:00:00.000Z","1961-01-01T00:00:00.000Z","1962-01-01T00:00:00.000Z","1963-01-01T00:00:00.000Z","1964-01-01T00:00:00.000Z","1965-01-01T00:00:00.000Z","1966-01-01T00:00:00.000Z","1967-01-01T00:00:00.000Z","1968-01-01T00:00:00.000Z","1969-01-01T00:00:00.000Z","1970-01-01T00:00:00.000Z","1971-01-01T00:00:00.000Z"],[49.9,52.3,49.4,51.1,49.4,47.9,49.8,50.9,49.3,51.9,50.8,49.6,49.3,50.6,48.4,50.7,50.9,50.6,51.5,52.8,51.8,51.1,49.8,50.2,50.4,51.6,51.8,50.9,48.8,51.7,51,50.6,51.7,51.5,52.1,51.3,51,54,51.4,52.7,53.1,54.6,52,52,50.9,52.6,50.2,52.6,51.6,51.9,50.5,50.9,51.7,51.4,51.7,50.8,51.9,51.8,51.9,53]]},"evals":[],"jsHooks":[]}
```
