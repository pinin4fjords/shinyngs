# Output function for the scatterplot module

This module uses [Plotly](https://plot.ly/) to create scatter plots (see
[`plot_ly`](https://rdrr.io/pkg/plotly/man/plot_ly.html)), of both 2D
and 3D varieties.

## Usage

``` r
scatterplotOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Controls for this module are provided by the `scatterplotcontrols`
module, which is automatically called if reactives are not supplied to
the server function. This setup allows the same set of controls to power
multiple scatter plots.

## Examples

``` r
scatterplotOutput("pca")
#> Error in scatterplotOutput("pca"): could not find function "scatterplotOutput"
```
