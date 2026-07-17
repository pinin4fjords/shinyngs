# The input function of the colorby module

This module provides a drop-down for picking an RColorBrewer color
palette and provides that palette given a reactive which supplied the
required number of colors.

## Usage

``` r
colormakerInput(id)
```

## Arguments

- id:

  Submodule namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This funcion provides the form elements to control the display

## Examples

``` r
colormakerInput("myid")
#> Error in colormakerInput("myid"): could not find function "colormakerInput"
```
