# Generate an integer ordering to select the n most variable genes out of a matrix

Generate an integer ordering to select the n most variable genes out of
a matrix

## Usage

``` r
selectVariableGenes(ntop, matrix = NULL, row_variances = NULL)
```

## Arguments

- ntop:

  Number of genes to select

- matrix:

  Matrix with genes by row and samples by column

- row_variances:

  Numeric vector of variances, in case a precalculated set of values
  should be used

## Value

output A vector of integers
