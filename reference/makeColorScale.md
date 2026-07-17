# Make a color palette of a specified length

Given an integer, make a palette with a specified number of colors using
palettes from RColorBrewer, and interpolation where necessary.

## Usage

``` r
makeColorScale(ncolors, palette = "Set1")
```

## Arguments

- ncolors:

  Integer specifying the number of colors

- palette:

  RColorBrewer palette name. (default: 'Set1')

## Value

output Character vector of colors

## Examples

``` r
makeColorScale(10)
#>  [1] "#999999" "#EC83BA" "#B75F49" "#E1C62F" "#FFB716" "#D16948" "#7E6E85"
#>  [8] "#48A462" "#4A72A6" "#E41A1C"
```
