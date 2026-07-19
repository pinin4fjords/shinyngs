# The UI input function of the experimenttable module

This module produces a simple table of the
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
in a SummarizedExperiment object. If more than one of these objects were
specified, a select box should appear to pick the desired one for
display.

## Usage

``` r
experimenttableInput(id, eselist)
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
#> Creating ExploratorySummarizedExperimentList object

experimenttableInput("experiment", eselist)
#> Error in experimenttableInput("experiment", eselist): could not find function "experimenttableInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepare_app("experimenttable", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
