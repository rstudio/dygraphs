# Add series data to dygraph

Add an additional column of series data to a dygraph. This is typically
used in the construction of custom series types (e.g. log scaled,
smoothed, etc.)

## Usage

``` r
dySeriesData(dygraph, name, values)
```

## Arguments

- dygraph:

  Dygraph to add data to

- name:

  Name of series

- values:

  Data values

## Value

Dygraph with additional series data
