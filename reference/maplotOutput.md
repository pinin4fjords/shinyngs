# The output function of the `maplot` module

This module produces an MA plot of log(10) expression vs log(2) fold
change for contrasts defined in the \`contrasts\` slot of an
'ExploratorySummarizedExperimentList\` object.

## Usage

``` r
maplotOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Leverages the `contrasts` and `scatterplot` modules

## Examples

``` r
maplotOutput("experiment")
#> Error in maplotOutput("experiment"): could not find function "maplotOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

if (interactive()) {
  app <- prepare_app("maplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
