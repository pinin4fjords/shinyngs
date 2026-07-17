# Make an interactive heatmap with heatmaply

This is a generic function which may be useful outside of this package.
It produces a heatmap based on an expression matrix and accompanying
experiment data in the form of a frame, using
[`heatmaply::heatmaply()`](https://talgalili.github.io/heatmaply/reference/heatmaply.html).

## Usage

``` r
interactiveHeatmap(
  plotmatrix,
  displaymatrix,
  sample_annotation,
  cluster_rows = TRUE,
  cluster_cols = FALSE,
  scale = "row",
  row_labels,
  colors = viridisLite::viridis(100),
  cexCol = 0.7,
  cexRow = 0.7,
  display_numbers = FALSE,
  hide_colorbar = FALSE,
  plot_height = 600,
  ...
)
```

## Arguments

- plotmatrix:

  Expression/ other data matrix

- displaymatrix:

  A matrix of values shown on hover, in addition to the (possibly
  scaled/ transformed) values in `plotmatrix`

- sample_annotation:

  A data frame with sample metadata, used to draw column side colors and
  an accompanying legend

- cluster_rows:

  Cluster rows?

- cluster_cols:

  Cluster columns?

- scale:

  'row', 'column' or none

- row_labels:

  Vector labels to use for rows

- colors:

  A vector of colors for the heatmap

- cexCol:

  Character expansion factor passed to `heatmaply()`

- cexRow:

  Character expansion factor passed to `heatmaply()`

- display_numbers:

  Boolean, should the (possibly scaled/ transformed) values in
  `plotmatrix` be displayed on the heatmap cells?

- hide_colorbar:

  Boolean, should the color scale legend be hidden?

- plot_height:

  The total rendered height of the plot in pixels, used to convert the
  fixed-pixel annotation row height into the fraction `heatmaply()`
  expects. Should match the `height` the plot is actually rendered at
  (e.g. the `height` argument of the `plotlyOutput()` it's displayed
  in).

- ...:

  Additional arguments passed to `heatmaply()`

## Value

output A plotly htmlwidget as produced by heatmaply()
