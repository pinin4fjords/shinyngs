# The server function of the genesetbarcodeplot module

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
genesetbarcodeplot(id, eselist)
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
if (interactive()) {
  data(airway, package = "airway")
  ese <- as(airway, "ExploratorySummarizedExperiment")
  eselist <- ExploratorySummarizedExperimentList(ese)
  genesetbarcodeplot("genesetbarcodeplot", eselist)

  # Almost certainly used via application creation

  app <- prepare_app("genesetbarcodeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
