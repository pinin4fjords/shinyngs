# Make scatterplots with `plot_ly()`

Make scatterplots with `plot_ly()`

## Usage

``` r
plotly_scatterplot(
  x,
  y,
  z = NULL,
  colorby = NULL,
  plot_type = "scatter",
  title = "",
  legend_title = "",
  xlab = "x",
  ylab = "y",
  zlab = "z",
  palette = NULL,
  point_size = 5,
  labels = NULL,
  show_labels = FALSE,
  lines = NULL,
  hline_thresholds = NULL,
  vline_thresholds = NULL,
  showlegend = TRUE,
  palette_name = COLORBLIND_PALETTE_NAME
)
```

## Arguments

- x:

  X coordinates

- y:

  Y coordinates

- z:

  Optional Z coordinates

- colorby:

  String vector or factor specifying value groups

- plot_type:

  Plot type: 'scatter' or 'scatter3d'

- title:

  Plot title

- legend_title:

  Legend title

- xlab:

  X label

- ylab:

  Y label

- zlab:

  Z label

- palette:

  Color palette correct for the number of groups in 'colorby'

- point_size:

  Main point size

- labels:

  Point labels

- show_labels:

  Permanently show labels for labelled points (default is just on
  hoverover)

- lines:

  3 column data-frame (name, x, y) with two rows, one for the start and
  end of each named line

- hline_thresholds:

  Named list of horizontal lines with y coordinates

- vline_thresholds:

  Named list of vertical lines x coordinates

- showlegend:

  Boolean: show a legend?

- palette_name:

  Valid R color palette name

## Value

output Plotly plot object
