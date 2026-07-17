# The input function of the heatmap module

Three types of heatmaps are provided, employed in various places in the
rnaseq app (for example), and using much of the same code. Expresssion
heatmaps plot expression for samples by column and e.g. genes by row. A
samples heatmap plots samples vs samples to illustrate correlation
patterns. A pca heatmap plots the results of anova tests applied to
examine the associations between principal components and experimental
variables.

## Usage

``` r
heatmapInput(id, eselist, type = "expression")
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- type:

  The type of heatmap that will be made. 'expression', 'samples' or
  'pca'

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This provides the form elements to control the heatmap display

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

heatmapInput("heatmap", eselist)
#> Error in heatmapInput("heatmap", eselist): could not find function "heatmapInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepareApp("heatmap", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
