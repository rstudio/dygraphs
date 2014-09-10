### dygraphs for R

The dygraphs package is an R interface to the [dygraphs](http://dygraphs.com) JavaScript charting library. It provides rich facilites for charting time-series data in R, including:

- Automatically plots [xts](http://cran.rstudio.com/web/packages/xts/index.html) time series objects (or any object convertible to xts).
- Rich interactive features including zoom, pan, and mouseover highlighting.
- Highly configurable series display, including the ability to display custom bars (e.g. - prediction intervals) around series.
- Usable at the R console as well as within [R Markdown](http://rmarkdown.rstudio.com) documents and [Shiny](http://shiny.rstudio.com) web applications.

#### Installation

The dygraphs package depends on the development version of the [htmlwidgets](https://github.com/ramnathv/htmlwidgets) package so you need to install both packages. You can do this using the **devtools** package as follows:

```S
devtools::install_github("ramnathv/htmlwidgets", "jjallaire/dygraphs")
```

#### Usage

If you have an xts-compatible time-series object creating an interactive plot of it is as simple as this:

```S
dygraph(nhtemp, main = "New Haven Temperatures")
```

You can also further customize axes and series display as well as add interacitve features like a range selector:

```S
dygraph(nhtemp, main = "New Haven Temperatures") %>%
  dyAxis("y", label = "Temp (F)", valueRange = c(40, 60)) %>%
  dyOptions(fillGraph = TRUE, drawGrid = FALSE) %>%
  dyRangeSelector()
```

See the [online documentation](http://jjallaire.github.io/dygraphs) for the dygraphs package for additional details and examples.









