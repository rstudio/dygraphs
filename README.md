### dygraphs for R

The dygraphs package is an R interface to the [dygraphs](http://dygraphs.com) JavaScript charting library. It provides rich facilites for charting time-series data in R, including:

- Automatically plots xts time series objects (or any object convertible to xts).
- Rich interactive features including zoom, pan, and mouseover highlighting.
- Highly configurable series display, including the ability to display custom bars (e.g. - prediction intervals) around series.
- Usable at the R console as well as within R Markdown documents and Shiny web applications.

#### Installation

The dygraphs package depends on the development version of the [htmlwidgets](https://github.com/ramnathv/htmlwidgets) package so you need to install both packages. You can do this using the **devtools** package as follows:

```S
devtools::install_github("ramnathv/htmlwidgets", "jjallaire/dygraphs")
```

