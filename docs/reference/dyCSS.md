# CSS for dygraph labels and legend

Apply custom CSS to the text drawn within a dygraph. See the [CSS
documentation](https://dygraphs.com/css.html) on the dygraphs website
for additional details on which styles are available.

## Usage

``` r
dyCSS(dygraph, css)
```

## Arguments

- dygraph:

  Dygraph to add CSS styles to

- css:

  Path to css file to be used for styling textual elements of the graph.

## Value

dygraph with additional CSS styles

## Note

See the [CSS
Styling](https://rstudio.github.io/dygraphs/gallery-css-styling.html)
article on the dygraphs for R website for additional details.

Note that CSS styles are global so will affect all dygraphs on a given
web page. This also implies that for a page with multiple plots you only
need to specify styles for the first one (alternatively you can just add
them directly to the page by other means).
