# The output function of the colorby module

This module provides a drop-down for picking an RColorBrewer color
palette and provides that palette given a reactive which supplied the
required number of colors.

## Usage

``` r
colormaker(id, getNumberCategories)
```

## Arguments

- id:

  Module namespace

- getNumberCategories:

  A reactive supplying the number of categories that require a color.

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
colormaker("myid", getNumberCategories)
#> Error in colormaker("myid", getNumberCategories): could not find function "colormaker"
```
