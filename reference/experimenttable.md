# The server function of the experimenttable module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).
Essentially this just passes the results of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
applied to the specified SummarizedExperiment object to the
`simpletable` module

## Usage

``` r
experimenttable(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

# Almost certainly used via application creation

if (interactive()) {
  experimenttable("experimenttable", eselist)
  app <- prepareApp("experimenttable", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
