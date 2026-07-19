# Make machine variable names pretty for display

Just split out words using '\_' and capitalise first letter

## Usage

``` r
prettify_variable_name(vn, tolower = FALSE)
```

## Arguments

- vn:

  A string to be prettified

- tolower:

  Convert to lower case first? (Default: FALSE)

## Value

output Prettified string

## Examples

``` r
vn <- "ugly_name_of_thing"
prettify_variable_name(vn)
#> [1] "Ugly name of thing"
```
