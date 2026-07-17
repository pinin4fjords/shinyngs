# Convert row names to metadata identifiers

Convert row names to metadata identifiers

## Usage

``` r
convertIds(ids, ese, to, remove_na = FALSE)
```

## Arguments

- ids:

  IDs found as row names in the `ExploratorySummarizedExperiment`

- ese:

  The `ExploratorySummarizedExperiment`

- to:

  The metadata column (via `mcols`) to use

- remove_na:

  Take out NAs? Not done by default to preserve vector length

## Value

output Vector of converted ids
