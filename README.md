# dygraphs for R

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/dygraphs)](https://cran.r-project.org/package=dygraphs)
[![R-CMD-check](https://github.com/rstudio/dygraphs/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/dygraphs/actions)
[![codecov](https://codecov.io/gh/rstudio/dygraphs/branch/master/graph/badge.svg?token=1z2BOSMfZe)](https://codecov.io/gh/rstudio/dygraphs)

<!-- badges: end -->

The `{dygraphs}` package is an R interface to the [dygraphs](https://dygraphs.com) JavaScript charting library. It provides rich facilities for charting time-series data in R, including:

- Automatically plots [xts](http://cran.rstudio.com/web/packages/xts/index.html) time series objects (or any object convertible to xts).

- Highly configurable axis and series display (including optional second Y-axis).

- Rich interactive features including [zoom/pan](https://rstudio.github.io/dygraphs/articles/gallery-range-selector.html) and series/point [highlighting](https://rstudio.github.io/dygraphs/articles/gallery-series-highlighting.html).

- Display [upper/lower bars](https://rstudio.github.io/dygraphs/articles/gallery-upper-lower-bars.html) (e.g. prediction intervals) around series.

- Various graph overlays including [shaded regions](https://rstudio.github.io/dygraphs/articles/gallery-shaded-regions.html), [event lines](https://rstudio.github.io/dygraphs/articles/gallery-event-lines.html), and point [annotations](https://rstudio.github.io/dygraphs/articles/gallery-annotations.html).

- Use at the R console just like conventional R plots (via RStudio Viewer).

- Seamless embedding within [R Markdown](https://rstudio.github.io/dygraphs/articles/dygraphs.html#rmarkdown) documents and [Shiny](https://rstudio.github.io/dygraphs/articles/dygraphs.html#shiny) web applications.

## Installation

You can install this package from CRAN, or the development version from GitHub:

``` r
# CRAN version
install.packages('dygraphs')

# Or Github version
if (!require('remotes')) install.packages('remotes')
remotes::install_github('rstudio/dygraphs')
```

## Usage

If you have an xts-compatible time-series object creating an interactive plot of it is as simple as this:

```r
dygraph(nhtemp, main = "New Haven Temperatures")
```

You can also further customize axes and series display as well as add interactive features like a range selector:

```r
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
  dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
  dyRangeSelector()
```

See the [online documentation](http://rstudio.github.io/dygraphs) for the `{dygraphs}` package for additional details and examples.
