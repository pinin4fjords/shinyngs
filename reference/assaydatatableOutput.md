# The output function of the assaydatatable module

This module displays the content of the currently selected experiment
and assay, also allowing grouping by mean etc.

## Usage

``` r
assaydatatableOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Leverages the `simpletable` module

## Examples

``` r
assaydatatableOutput("experiment")
#> Error in assaydatatableOutput("experiment"): could not find function "assaydatatableOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

if (interactive()) {
  app <- prepareApp("assaydatatable", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
