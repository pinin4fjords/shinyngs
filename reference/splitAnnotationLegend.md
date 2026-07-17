# Replace heatmaply's combined annotation legend with one split by variable

By default heatmaply shows a single generically-titled legend/colorbar
covering every value of every `col_side_colors` variable pooled
together. This hides that combined colorbar and adds one invisible,
legend-only trace per value of each annotation variable instead, grouped
and titled by variable name, using the same colors as the annotation
bars.

## Usage

``` r
splitAnnotationLegend(p, col_side_colors, palette, ncol_heatmap)
```

## Arguments

- p:

  A plotly htmlwidget as produced by
  [`heatmaply::heatmaply()`](https://talgalili.github.io/heatmaply/reference/heatmaply.html)
  with `col_side_colors` supplied

- col_side_colors:

  A data frame with sample metadata, as passed to
  [`heatmaply::heatmaply()`](https://talgalili.github.io/heatmaply/reference/heatmaply.html)

- palette:

  A named vector of colors as produced by
  [`combinedAnnotationColors`](https://pinin4fjords.github.io/shinyngs/reference/combinedAnnotationColors.md)

- ncol_heatmap:

  Number of columns in the main heatmap data, used to identify the
  annotation strip trace(s) among the plot's other traces

## Value

output The modified plotly htmlwidget
