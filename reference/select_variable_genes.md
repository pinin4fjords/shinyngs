# Generate an integer ordering to select the n most variable genes out of a matrix

Generate an integer ordering to select the n most variable genes out of
a matrix

## Usage

``` r
select_variable_genes(ntop, matrix = NULL, row_variances = NULL)
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

## Examples

``` r
mat <- matrix(rnorm(60), nrow = 15, ncol = 4,
  dimnames = list(paste0("gene", 1:15), paste0("s", 1:4)))
select_variable_genes(ntop = 5, matrix = mat)
#> [1]  6  1 14 10 15
```
