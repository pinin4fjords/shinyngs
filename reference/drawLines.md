# Overlay lines on a plotly-generated plot

Overlay lines on a plotly-generated plot

## Usage

``` r
drawLines(
  p,
  x,
  y,
  lines = NULL,
  hline_thresholds = list(),
  vline_thresholds = list(),
  plot_type = "scatter",
  xrange = NULL,
  yrange = NULL
)
```

## Arguments

- p:

  Previously generated plotly object

- x:

  X coordinates of points, used to determine x range

- y:

  Y coordinates of points, used to determine y range

- lines:

  3 column data-frame (name, x, y) with two rows, one for the start and
  end of each named line

- hline_thresholds:

  Alternatively or in addition to 'lines', just specify a named list of
  y values at which to place hlines

- vline_thresholds:

  Alternatively or in addition to 'lines', just specify a named list of
  x values at which to place vlines

- plot_type:

  Plot type: 'scatter' or 'scatter3d'. The axis range fix that makes
  threshold lines reach the plot edges only applies to 2D plots.

- xrange:

  Optional fixed c(min, max) x axis range. When NULL (the default) the
  range is derived from the data with 5% padding.

- yrange:

  Optional fixed c(min, max) y axis range. When NULL (the default) the
  range is derived from the data with 5% padding.

## Value

output Plotly object
