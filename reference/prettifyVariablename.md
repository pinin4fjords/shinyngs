# Make machine variable names pretty for display

Just split out words using '\_' and capitalise first letter

## Usage

``` r
prettifyVariablename(vn, tolower = FALSE)
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
prettyifyVariablename(vn)
#> Error in prettyifyVariablename(vn): could not find function "prettyifyVariablename"
```
