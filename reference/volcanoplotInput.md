# The UI input function of the `volcanoplot` module

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
volcanoplotInput(id, eselist)
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

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object

# The volcano module needs an eselist carrying differential statistics
# (contrast_stats); see the vignette. It is used via application creation:

if (interactive()) {
  volcanoplotInput("myid", eselist)
  app <- prepareApp("volcanoplot", eselist)
  shiny::shinyApp(ui = app$ui, server = app$server)
}
```
