# Add links to a table

Root URLs must be present in the `url_roots` slot of `se`

## Usage

``` r
linkMatrix(matrix, url_roots, display_values = data.frame())
```

## Arguments

- matrix:

  The input table

- url_roots:

  A list with URL roots, with names matching columns of `matrix`

- display_values:

  A matrix which may contain values for displaying in the link,
  differerent from that used in the href.

## Value

output Table with links added
