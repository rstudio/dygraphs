# Candlestick Charts

You can create candlestick/OHLC charts with
[`dyCandlestick()`](https://rstudio.github.io/dygraphs/reference/Plotters.md)
function. For example:

``` r

library(xts)
data(sample_matrix)
m <- tail(sample_matrix, n = 32)
dygraph(m) |>
  dyCandlestick()
```

Candlestick charts use the first four data series to plot, the rest of
the data series (if any) are rendered with line plotter:

``` r

m <- cbind(m, apply(m[, 1:3], 1, mean))
colnames(m)[5] <- "Mean"
dygraph(m) |>
  dyCandlestick()
```

You can also use `compress` function argument to compress chart data
annually, quarterly, monthly, weekly or daily depending on the current
chart zoom level to prevent chart bars overflow:

``` r

library(xts)
data(sample_matrix)
dygraph(sample_matrix) |>
  dyCandlestick(compress = TRUE)
```
