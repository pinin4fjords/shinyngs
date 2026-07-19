# Replace NAs with a string for convenience

Replace NAs with a string for convenience

## Usage

``` r
na.replace(vec, replacement = "NA")
```

## Arguments

- vec:

  Character vector or factor containing NAs

- replacement:

  Character replacement (default: 'NA')

## Value

Vector or factor with NAs replaced

## Examples

``` r
na.replace(c("a", NA, "b"))
#> [1] "a"  "NA" "b" 
```
