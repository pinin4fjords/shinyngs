# The UI input function of the simpletable module

This module produces a simple datatable output with a download button.

## Usage

``` r
simpletableInput(id, tabletitle = "", description = NULL)
```

## Arguments

- id:

  Submodule namespace

- tabletitle:

  Table title. Will be used on download button.

- description:

  A string to display in the side bar to explain the table.

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
simpletableInput("mytable", "this is a table")
#> Error in simpletableInput("mytable", "this is a table"): could not find function "simpletableInput"
```
