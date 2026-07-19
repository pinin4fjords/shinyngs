# Summarise the rows of a matrix, applying a function to groups of cells defined by a factor

Note that the specified function will be applied to a tranformed version
of the matrix, so [`colMeans()`](https://rdrr.io/r/base/colSums.html),
for example, is appropriate.

## Usage

``` r
summarize_matrix(matrix, treatment_factor, summaryFunc = "colMeans")
```

## Arguments

- matrix:

  Numeric matrix

- treatment_factor:

  a factor defining column groups

- summaryFunc:

  A Function to apply to a transformed version of `matrix` (default:
  colMeans)

## Value

Summarized matrix, with e.g. means in columns

## Examples

``` r
summarize_matrix(mymatrix, myfactor)
#> Error: object 'myfactor' not found
```
