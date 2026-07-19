# Make the bar chart illustrating intersect size

Make the bar chart illustrating intersect size

## Usage

``` r
upset_intersect_size_chart(ints, nintersects, bar_numbers = FALSE)
```

## Arguments

- ints:

  A list with `combinations` and `intersections`, as returned by
  [`upset_calculate_intersections`](https://pinin4fjords.github.io/shinyngs/reference/upset_calculate_intersections.md)
  (optionally filtered by
  [`upset_filter_intersections_by_order`](https://pinin4fjords.github.io/shinyngs/reference/upset_filter_intersections_by_order.md))

- nintersects:

  Maximum number of intersections to display

- bar_numbers:

  Add value labels above the bars?

## Value

output A plotly htmlwidget
