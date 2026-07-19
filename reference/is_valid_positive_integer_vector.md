# Check if a comma-separated string can be parsed to an integer vector

Check if a comma-separated string can be parsed to an integer vector

## Usage

``` r
is_valid_positive_integer_vector(string)
```

## Arguments

- string:

  Input string

## Value

Boolean indicating whether the check passed

## Examples

``` r
is_valid_positive_integer_vector("1,2,3")
#> [1] TRUE
is_valid_positive_integer_vector("1,two,3")
#> [1] FALSE
```
