# Include a dygraph plugin

Include a dygraph plugin

## Usage

``` r
dyPlugin(dygraph, name, path, options = list(), version = "1.0")
```

## Arguments

- dygraph:

  Dygraph to add plugin to

- name:

  Name of plugin

- path:

  Path to plugin JavaScript file

- options:

  Named list of options to pass to plugin constructor

- version:

  Plugin version (e.g. version of package which provides the plugin)

## Value

A dygraph with the specified plugin enabled.

## Details

You can use dygraphs plugins to customize the appearance of dygraphs as
well as add new interactive behaviors. For additional information on
creating dygraphs plugins see the [dygraphs
plugins](https://github.com/danvk/dygraphs/tree/master/src/plugins)
documentation. Once you've created a dygraphs plugins you can use the
dyPlugin function to create an R wrapper for it. See
<https://rstudio.github.io/dygraphs/gallery-plugins.html> for details on
how to do this.
