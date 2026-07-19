# Compute set intersections and sizes for an UpSet-style plot

Enumerates every combination of the supplied sets (at every order from
single sets up to all of them), and sizes each one. Under
`intersection_assignment_type = "upset"` a member is only counted at its
highest-order intersection (as in the original UpSet), so lower-order
combinations only report members not already claimed by a higher one;
under `"all"` a member is counted in every intersection it belongs to.

## Usage

``` r
upset_calculate_intersections(
  sets,
  show_empty_intersections = TRUE,
  intersection_assignment_type = "upset"
)
```

## Arguments

- sets:

  A named list of character vectors, one per set (e.g. gene identifiers)

- show_empty_intersections:

  Include intersections/set combinations with zero members?

- intersection_assignment_type:

  'upset' or 'all', see above

## Value

output A list with `combinations` (a list of integer vectors, each
giving the positions in `sets` making up one intersection) and
`intersections` (the matching sizes), ordered by size, descending
