# The output function of the genesetbarcodeplot module

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
genesetbarcodeplotOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
genesetbarcodeplotOutput("experiment")
#> Error in genesetbarcodeplotOutput("experiment"): could not find function "genesetbarcodeplotOutput"

# Almost certainly used via application creation

if (interactive()) {
  data(airway, package = "airway")
  ese <- as(airway, "ExploratorySummarizedExperiment")
  eselist <- ExploratorySummarizedExperimentList(ese)
  app <- prepare_app("genesetbarcodeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
