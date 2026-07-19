# Interleave the columns of two matrices of equal dimensions

Interleave the columns of two matrices of equal dimensions

## Usage

``` r
interleave_columns(mat1, mat2)
```

## Arguments

- mat1:

  First numeric matrix

- mat2:

  Second numeric matrix

## Value

output Interleaved matrix

## Examples

``` r
mat1 <- matrix(1:4, nrow = 2, dimnames = list(NULL, c("a1", "a2")))
mat2 <- matrix(5:8, nrow = 2, dimnames = list(NULL, c("b1", "b2")))
interleave_columns(mat1, mat2)
#>      a1 b1 a2 b2
#> [1,]  1  5  3  7
#> [2,]  2  6  4  8
```
