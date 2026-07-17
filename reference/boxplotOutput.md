# The output function of the boxplot module

This module produces displays of the distributions of the values in the
selected assay matrix. For low sample numbers (\<= 20) the default is a
boxplot produced using `ggplot2`. For higher sample numbers, the default
is a line-based alternative using `plotly`.

## Usage

``` r
boxplotOutput(id)
```

## Arguments

- id:

  Submodule namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
boxplotOutput("boxplot")
#> Error in boxplotOutput("boxplot"): could not find function "boxplotOutput"

# Almost certainly used via application creation

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

if (interactive()) {
  app <- prepareApp("boxplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
