# The output function of the pca module

This provides a shiny `tabsetPanel` with `tabPanel`s for both components
and loading plots.

## Usage

``` r
pcaOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
pcaOutput("pca")
#> Error in pcaOutput("pca"): could not find function "pcaOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

if (interactive()) {
  app <- prepareApp("pca", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
