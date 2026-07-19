# The UI input function of the `foldchangeplot` module

This module is for making scatter plots comparing pairs of groups
defined in a 'contrasts' slot of the ExploratorySummarizedExperimentList

## Usage

``` r
foldchangeplotInput(id, eselist)
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

## Details

Leverages the `scatterplot` module

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

foldchangeplotInput("myid", eselist)
#> Error in foldchangeplotInput("myid", eselist): could not find function "foldchangeplotInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepareApp("foldchangeplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
