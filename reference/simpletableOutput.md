# The output function of the simpletable module

This provides actual datatable element for display by applications

## Usage

``` r
simpletableOutput(id, tabletitle = NULL)
```

## Arguments

- id:

  Module namespace

- tabletitle:

  (optional) Title to display with the table

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
simpletableOutput("simpletable", "my title")
#> Error in simpletableOutput("simpletable", "my title"): could not find function "simpletableOutput"
```
