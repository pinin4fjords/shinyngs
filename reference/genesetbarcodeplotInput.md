# The UI input function of the genesetbarcodeplot module

This module leverages gene sets stored in the `gene_sets` slot of an
`ExploratorySummarizedExperimentList` object to produce interactive
barcode plots, reproducing the statistics behind Limma's
[`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html)
function. Genes are ranked by fold changes calculated with the
`contrasts` module, and FDR values from the `gene_set_analyses` slot of
the selected `ExploratorySummarizedExperiment` are displayed where
provided.

## Usage

``` r
genesetbarcodeplotInput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

# The gene set modules need an eselist with populated gene_set_analyses (see
# the vignette). Given such data, they are used via application creation:

if (interactive()) {
  genesetbarcodeplotInput("myid", eselist)
  app <- prepareApp("genesetbarcodeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
