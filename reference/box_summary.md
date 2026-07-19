# Summarise a vector into the statistics a box plot needs

Computes the quartiles, Tukey-style whisker extents (the most extreme
observations still within `whisker_distance` IQRs of the box), and the
outliers lying beyond them. This is the server-side reduction that lets
[`interactive_boxplot`](https://pinin4fjords.github.io/shinyngs/reference/interactive_boxplot.md)
draw genuine box glyphs while sending only a handful of numbers per box
to the browser rather than every observation.

## Usage

``` r
box_summary(values, labels, whisker_distance = 1.5)
```

## Arguments

- values:

  Numeric vector

- labels:

  Character vector of point labels, parallel to `values`

- whisker_distance:

  IQR multiplier for the whiskers (see `coef` in
  [`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html))

## Value

A list with scalar `q1`, `median`, `q3`, `lowerfence` and `upperfence`,
plus `outlier_values` and `outlier_labels` vectors
