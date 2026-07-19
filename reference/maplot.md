# The server function of the `maplot` module

This module is for making scatter plots comparing pairs of groups
defined in a 'contrasts' slot of the ExploratorySummarizedExperiment

## Usage

``` r
maplot(id, eselist)
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
#> Creating ExploratorySummarizedExperimentList object

# Almost certainly used via application creation

if (interactive()) {
  maplot("maplot", eselist)
  app <- prepareApp("maplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
