# The server function of the boxplot module

This module produces displays of the quartiles of the values in the
selected assay matrix. For low sample numbers (\<= 20) the default is a
boxplot produced using `ggplot2`. For higher sample numbers, the default
is a line-based alternative using `plotly`.

## Usage

``` r
boxplot(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

# Almost certainly used via application creation

if (interactive()) {
  boxplot("boxplot", eselist)
  app <- prepareApp("boxplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
