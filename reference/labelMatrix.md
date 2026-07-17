# Add columns to display ID and label in a table

Labels only added if `labelfield` is specified in `ese`

## Usage

``` r
labelMatrix(matrix, ese, idcol = NULL, metafields = c())
```

## Arguments

- matrix:

  The input table

- ese:

  An ExploratorySummarizedExperiment object

- idcol:

  ID column in the matrix, NULL to use row names

- metafields:

  Vector of metadata columns to add (via `mcols()`)

## Value

output Table with columns added
