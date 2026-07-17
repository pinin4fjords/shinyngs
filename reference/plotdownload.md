# The server function of the gene set module

The plotdownload module provides export functionality for panels with
plots. This will generally not be called directly, but by other modules

## Usage

``` r
plotdownload(id, makePlot, filename = "plot.png", plotHeight, plotWidth)
```

## Arguments

- id:

  Module namespace

- makePlot:

  A reactive for generating the plot

- filename:

  A filename (default = 'plot.png')

- plotHeight:

  A number or reactive for calculating the height of the plot

- plotWidth:

  A number or reactive for calculating the width of the plot

## Examples

``` r
plotdownload("heatmap", makePlot = plotHeatmap, filename = "heatmap.png", plotHeight = plotHeight, plotWidth = plotWidth)
#> Error in plotdownload("heatmap", makePlot = plotHeatmap, filename = "heatmap.png",     plotHeight = plotHeight, plotWidth = plotWidth): could not find function "plotdownload"
```
