# Create row labels based on the settings of `labelfield` in the `ExploratorySummarizedExperiment` object and the annotation data in `mcols`.

Create row labels based on the settings of `labelfield` in the
`ExploratorySummarizedExperiment` object and the annotation data in
`mcols`.

## Usage

``` r
idToLabel(ids, ese, sep = " / ")
```

## Arguments

- ids:

  list of ids

- ese:

  An ExploratorySummarizedExperiment

- sep:

  Separator for ID and label fields

## Value

String vector of same length as `ids`
