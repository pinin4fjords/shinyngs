# Geometric means by matrix column

Geometric means by matrix column

## Usage

``` r
colGeomMeans(x)
```

## Arguments

- x:

  A matrix

## Value

Vector with column geometric means

## Examples

``` r
m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
colGeomMeans(m)
#>        a        b        c 
#> 1.414214 3.464102 5.477226 
```
