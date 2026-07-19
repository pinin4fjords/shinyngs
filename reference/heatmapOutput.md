# The output function of the heatmap module

Three types of heatmaps are provided, employed in various places in the
rnaseq app (for example), and using much of the same code. Expresssion
heatmaps plot expression for samples by column and e.g. genes by row. A
samples heatmap plots samples vs samples to illustrate correlation
patterns. A pca heatmap plots the results of anova tests applied to
examine the associations between principal components and experimental
variables.

## Usage

``` r
heatmapOutput(id, type = "")
```

## Arguments

- id:

  Submodule namespace

- type:

  Heatmap type: 'pca', 'samples' or 'expression'

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
heatmapOutput("heatmap")
#> Error in heatmapOutput("heatmap"): could not find function "heatmapOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

if (interactive()) {
  app <- prepareApp("heatmap", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
