# Add permanent text labels to points in a plotly graph

Add permanent text labels to points in a plotly graph

## Usage

``` r
addTextLabels(
  p,
  x,
  y,
  z,
  colorby = NULL,
  labels,
  plot_type,
  show_labels = TRUE
)
```

## Arguments

- p:

  Previously generated plotly object

- x:

  Vector of numeric x values

- y:

  Vector of numeric y values

- z:

  Optional vector of numeric z values

- colorby:

  String vector or factor specifying value groups

- labels:

  Vector of labels to apply

- plot_type:

  Plot type: 'scatter' or 'scatter3d'

- show_labels:

  If false, simpy pass through input plot object

## Value

output Plotly object
