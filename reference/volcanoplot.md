# The server function of the `volcanoplot` module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).
Essentially this just passes the results of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
applied to the specified SummarizedExperiment object to the
`simpletable` module

## Usage

``` r
volcanoplot(id, eselist)
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
#> Creating ExploratorySummarizedExperimentList object

# However, almost certainly called via application creation:

if (interactive()) {
  differentialtable("differentialtable", eselist)
  app <- prepareApp("volcanoplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
