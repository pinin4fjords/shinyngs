# Simplify a contrast table

By default the contrast tables are created with three initial columns to
indicate the contrast: the metadata variable and the two values of that
variable that define the contrast. But if there is only one contrast
then this make the table overly cumbersome, an we can simplify it by
simply naming the average column to the values of the contrast variable.

## Usage

``` r
simplifyContrastTable(table)
```

## Arguments

- table:

  Three-column contrast table

## Value

output Simplified table
