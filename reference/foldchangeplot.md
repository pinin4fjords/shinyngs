# The server function of the `foldchangeplot` module

This module is for making scatter plots comparing pairs of groups
defined in a 'contrasts' slot of the ExploratorySummarizedExperimentList

## Usage

``` r
foldchangeplot(id, eselist)
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

if (interactive()) {
  foldchangeplot("foldchangeplot", eselist)
  app <- prepareApp("foldchangeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
