# The output function of the simpletable module

This provides actual datatable element for display by applications

## Usage

``` r
simpletableOutput(id, tabletitle = NULL, spinner = FALSE)
```

## Arguments

- id:

  Module namespace

- tabletitle:

  (optional) Title to display with the table

- spinner:

  Show a loading spinner while the table is (re)computed. Intended for
  tables backed by expensive reactives (e.g. differential expression);
  left off by default for small, near-instant tables.

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
simpletableOutput("simpletable", "my title")
#> Error in simpletableOutput("simpletable", "my title"): could not find function "simpletableOutput"
```
