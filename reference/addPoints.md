# Add points to a plotly object

Add points to a plotly object

## Usage

``` r
addPoints(
  p,
  x,
  y,
  z = NULL,
  colorby = NULL,
  name = NULL,
  label = FALSE,
  plot_type = "scatter",
  point_size = 5,
  labels = NULL,
  showlegend = FALSE
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

- name:

  Name for the series

- label:

  Boolean- should points be colored and labelled?

- plot_type:

  Plot type: 'scatter' or 'scatter3d'

- point_size:

  Main point size

- labels:

  Vector of labels to apply (if 'label' is TRUE)

- showlegend:

  Boolean: show this set of points in the legend?

## Value

output Plotly plot object
