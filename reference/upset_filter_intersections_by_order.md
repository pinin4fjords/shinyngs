# Drop intersections involving fewer than a minimum number of sets

Drop intersections involving fewer than a minimum number of sets

## Usage

``` r
upset_filter_intersections_by_order(ints, minorder)
```

## Arguments

- ints:

  A list with `combinations` and `intersections`, as returned by
  [`upset_calculate_intersections`](https://pinin4fjords.github.io/shinyngs/reference/upset_calculate_intersections.md)

- minorder:

  Minimum number of sets that must be involved in an intersection for it
  to be kept

## Value

output `ints`, with both elements filtered to the same subset
