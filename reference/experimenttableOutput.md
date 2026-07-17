# The output function of the experimenttable module

This module produces a simple table of the
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
in an ExploratorySummarizedExperiment object. If more than one of these
objects were specified, a select box should appear to pick the desired
one for display.

## Usage

``` r
experimenttableOutput(id)
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
experimenttableOutput("experiment")
#> Error in experimenttableOutput("experiment"): could not find function "experimenttableOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

if (interactive()) {
  app <- prepareApp("experimenttable", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
