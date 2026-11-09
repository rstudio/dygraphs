# Straw Broom Charts

You can create straw broom charts with
[`dyRebase()`](https://rstudio.github.io/dygraphs/reference/dyRebase.md)
function. For example:

``` r

library(quantmod)

tickers <- c("AAPL", "MSFT")
getSymbols(tickers)
closePrices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
dateWindow <- c("2008-01-01", "2009-01-01")

dygraph(closePrices, main = "Value", group = "stock") |>
  dyRebase(value = 100) |>
  dyRangeSelector(dateWindow = dateWindow)

dygraph(closePrices, main = "Percent", group = "stock") |>
  dyRebase(percent = TRUE) |>
  dyRangeSelector(dateWindow = dateWindow)

dygraph(closePrices, main = "None", group = "stock") |>
  dyRangeSelector(dateWindow = dateWindow)
```

    ## Loading required package: xts

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    ## Loading required package: TTR

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ## [1] "AAPL" "MSFT"
