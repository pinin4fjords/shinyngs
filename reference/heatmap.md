# The server function of the heatmap module

Three types of heatmaps are provided, employed in various places in the
rnaseq app (for example), and using much of the same code. Expresssion
heatmaps plot expression for samples by column and e.g. genes by row. A
samples heatmap plots samples vs samples to illustrate correlation
patterns. A pca heatmap plots the results of anova tests applied to
examine the associations between principal components and experimental
variables.

## Usage

``` r
heatmap(id, eselist, type = "expression")
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- type:

  The type of heatmap that will be made. 'expression', 'samples' or
  'pca' (default: 'expression')

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
  heatmap("heatmap", eselist, type = "pca")
  app <- prepare_app("heatmap", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
