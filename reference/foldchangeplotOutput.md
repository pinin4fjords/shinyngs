# The output function of the `foldchangeplot` module

This module is for making scatter plots comparing pairs of groups
defined in a 'contrasts' slot of the ExploratorySummarizedExperimentList

## Usage

``` r
foldchangeplotOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Leverages the `scatterplot` module

## Examples

``` r
foldchangeplotOutput("myid")
#> Error in foldchangeplotOutput("myid"): could not find function "foldchangeplotOutput"

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

if (interactive()) {
  app <- prepareApp("foldchangeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
