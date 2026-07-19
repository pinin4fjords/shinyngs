# Colour points by a single variable, highlighting labelled rows

Builds the labelled/unselected trace pair used by `plotly_scatterplot`
when no `colorby_menu` is supplied: unlabelled rows are drawn as a grey
background layer, and rows with a non-NA label are coloured by `colorby`
and optionally given permanent text labels.

## Usage

``` r
addColoredPoints(
  x,
  y,
  z = NULL,
  colorby = NULL,
  plot_type = "scatter",
  palette = NULL,
  palette_name = COLORBLIND_PALETTE_NAME,
  point_size = 5,
  labels = NULL,
  show_labels = FALSE,
  showlegend = TRUE
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

- palette:

  Color palette correct for the number of groups in 'colorby'

- palette_name:

  Valid R color palette name, used when 'palette' is NULL

- point_size:

  Main point size

- labels:

  Point labels

- show_labels:

  Permanently show labels for labelled points (default is just on
  hoverover)

- showlegend:

  Boolean: show a legend?

## Value

output Plotly plot object
