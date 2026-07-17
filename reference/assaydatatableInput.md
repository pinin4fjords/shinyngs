# The UI input function of the assaydatatable module

This module displays the content of the currently selected experiment
and assay, also allowing grouping by mean etc.

## Usage

``` r
assaydatatableInput(id, eselist)
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

Leverages the `simpletable` module

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

assaydatatableInput("experiment", eselist)
#> Error in assaydatatableInput("experiment", eselist): could not find function "assaydatatableInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepareApp("assaydatatable", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
