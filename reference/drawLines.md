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
  vline_thresholds = list()
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

## Value

output Plotly object
