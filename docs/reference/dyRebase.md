# Rebase data handler for straw broom charts with Dygraph

Draw a straw broom chart.

## Usage

``` r
dyRebase(dygraph, value = 100, percent = FALSE)
```

## Arguments

- dygraph:

  Dygraph to draw chart on

- value:

  Value to rebase to

- percent:

  If true, ingnore value argument and rebase to percentage difference

## Value

Dygraph with specified straw broom chart

## Examples

``` r
if (FALSE) { # \dontrun{
library(quantmod)
tickers <- c("AAPL", "MSFT")
getSymbols(tickers)
closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
dateWindow <- c("2008-01-01", "2009-01-01")
dygraph(closePrices, main = "Value", group = "stock") %>%
  dyRebase(value = 100) %>%
  dyRangeSelector(dateWindow = dateWindow)
dygraph(closePrices, main = "Percent", group = "stock") %>%
  dyRebase(percent = TRUE) %>%
  dyRangeSelector(dateWindow = dateWindow)
dygraph(closePrices, main = "None", group = "stock") %>%
  dyRangeSelector(dateWindow = dateWindow)
} # }
```
