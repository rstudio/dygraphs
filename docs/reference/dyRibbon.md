# dyRibbon plugin adds a horizontal band of colors that runs through the chart. It can be useful to visualize categorical variables (http://en.wikipedia.org/wiki/Categorical_variable) that change over time (along the x-axis).

dyRibbon plugin adds a horizontal band of colors that runs through the
chart. It can be useful to visualize categorical variables
(http://en.wikipedia.org/wiki/Categorical_variable) that change over
time (along the x-axis).

## Usage

``` r
dyRibbon(
  dygraph,
  data = NULL,
  palette = NULL,
  parser = NULL,
  top = 1,
  bottom = 0
)
```

## Arguments

- dygraph:

  Dygraph to add plugin to

- data:

  Vector of numeric values in the range from 0 to 1

- palette:

  Vector with colors palette

- parser:

  JavaScrip function (function (data, dygraph)) returning the array of
  numeric values. Parser is used if no data was provided

- top:

  Vertical position of the top edge of ribbon relative to chart height.

- bottom:

  Vertical position of the bottom edge of ribbon relative to chart
  height.

## Examples

``` r
if (FALSE) { # \dontrun{
library(quantmod)

getSymbols("SPY", from = "2016-12-01", auto.assign = TRUE)

difference <- SPY[, "SPY.Open"] - SPY[, "SPY.Close"]
decreasing <- which(difference < 0)
increasing <- which(difference > 0)

dyData <- SPY[, "SPY.Close"]

ribbonData <- rep(0, nrow(dyData))
ribbonData[decreasing] <- 0.5
ribbonData[increasing] <- 1

dygraph(dyData) %>%
  dyRibbon(data = ribbonData, top = 0.1, bottom = 0.02)
} # }
```
