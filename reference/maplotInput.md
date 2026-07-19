# The UI input function of the `maplot` module

This module produces an MA plot of log(10) expression vs log(2) fold
change for contrasts defined in the \`contrasts\` slot of an
'ExploratorySummarizedExperimentList\` object.

## Usage

``` r
maplotInput(id, eselist)
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

Leverages the `contrasts` and `scatterplot` modules

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

maplotInput("myid", eselist)
#> Error in maplotInput("myid", eselist): could not find function "maplotInput"

# Almost certainly used via application creation

if (interactive()) {
  app <- prepare_app("maplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
