# The output function of the colorby module

Supplies a reactive returning shinyngs' fixed colour-blind-safe palette
(see
[`makeColorScale`](https://pinin4fjords.github.io/shinyngs/reference/makeColorScale.md)),
sized to the number of categories the caller reports.

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

output A reactive returning a character vector of hex colors

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
colormaker("myid", getNumberCategories)
#> Error in colormaker("myid", getNumberCategories): could not find function "colormaker"
```
