# The output function of the colorby module

Supplies a reactive returning the palette selected in the drop-down,
sized to the number of categories the caller reports (see
[`make_color_scale`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md)).

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
