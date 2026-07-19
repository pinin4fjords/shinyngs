# The input function of the pca module

This provides the form elements to control the pca display, derived from
the `selectmatrix`, `scatterplotcontrols`, and `scatterplot` modules.

## Usage

``` r
pcaInput(id, eselist)
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

pcaInput("pca", eselist)
#> Error in pcaInput("pca", eselist): could not find function "pcaInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepareApp("pca", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
