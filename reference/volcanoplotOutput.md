# The output function of the `volcanoplot` module

A volcano plot displays -log(10) of a p value/ FDR against a log(2) fold
change on the x axis. This module produces such a plot using the
[`scatterplot`](https://pinin4fjords.github.io/shinyngs/reference/scatterplot.md)
module (which uses
[`plot_ly`](https://rdrr.io/pkg/plotly/man/plot_ly.html))), using data
provided by the
[`contrasts`](https://pinin4fjords.github.io/shinyngs/reference/contrasts.md)
module based on the setting of `contrasts` in `eselist`.

## Usage

``` r
volcanoplotOutput(id)
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
differentialtableOutput("experiment")
#> Error in differentialtableOutput("experiment"): could not find function "differentialtableOutput"

# However, almost certainly called via application creation:

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

if (interactive()) {
  app <- prepareApp("volcanoplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
