# The server function of the pca module

This module calculates a PCA and formats components and loadings for
display. It uses a common set of controls, generated with the
`scatterplotcontrols` module, to power scatter plots for both components
and loadings produced by the `scatterplots` module.

## Usage

``` r
pca(id, eselist)
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

Matrix and UI selection elements provided by the selectmatrix module

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

# Almost certainly used via application creation

if (interactive()) {
  pca("pca", eselist)
  app <- prepare_app("pca", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
