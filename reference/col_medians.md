# Medians by matrix column

Medians by matrix column

## Usage

``` r
col_medians(x)
```

## Arguments

- x:

  A matrix

## Value

Vector with column medians

## Examples

``` r
m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
col_medians(m)
#>   a   b   c 
#> 1.5 3.5 5.5 
```
