# Make the grid of points indicating set membership in intersections

Make the grid of points indicating set membership in intersections

## Usage

``` r
upset_grid_plot(sets, ints, nintersects)
```

## Arguments

- sets:

  A named list of character vectors, one per set

- ints:

  A list with `combinations` and `intersections`, as returned by
  [`upset_calculate_intersections`](https://pinin4fjords.github.io/shinyngs/reference/upset_calculate_intersections.md)
  (optionally filtered by
  [`upset_filter_intersections_by_order`](https://pinin4fjords.github.io/shinyngs/reference/upset_filter_intersections_by_order.md))

- nintersects:

  Maximum number of intersections to display, taken from the front of
  `ints`

## Value

output A plotly htmlwidget
