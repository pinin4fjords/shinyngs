# Make a categorical colour scale of a specified length

With the default `palette = "colorblind"` (or `NULL`), returns colours
drawn, in order, from shinyngs' fixed colour-blind-safe categorical
palette
([`COLORBLIND_PALETTE`](https://pinin4fjords.github.io/shinyngs/reference/COLORBLIND_PALETTE.md)),
so the same position always gets the same colour and a group keeps its
colour across plots; when more colours are requested than the base
palette provides, additional shades are interpolated between them and a
message is emitted, since colour-blind separation can no longer be
guaranteed for every pair. Any other value is treated as an RColorBrewer
palette name and expanded with interpolation where necessary.

## Usage

``` r
makeColorScale(ncolors, palette = COLORBLIND_PALETTE_NAME)
```

## Arguments

- ncolors:

  Integer specifying the number of colors

- palette:

  `"colorblind"` (or `NULL`) for the colour-blind-safe palette,
  otherwise an RColorBrewer palette name. (default: 'colorblind')

## Value

output Character vector of colors

## Examples

``` r
makeColorScale(10)
#> makeColorScale: 10 categories requested, more than the 8 colours in shinyngs' colour-blind-safe palette. Interpolating additional shades, which will be harder to tell apart than the base palette.
#>  [1] "#E69F00" "#76AFB5" "#26A7A7" "#50B562" "#D5D74E" "#1A7EA5" "#8E643B"
#>  [8] "#D1694A" "#B27195" "#595959"
```
