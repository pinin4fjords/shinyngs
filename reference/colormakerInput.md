# The input function of the colorby module

Provides a drop-down for picking the plot colour palette. The default is
shinyngs' fixed colour-blind-safe palette (see
[`make_color_scale`](https://pinin4fjords.github.io/shinyngs/reference/make_color_scale.md));
the remaining choices are RColorBrewer qualitative palettes for users
who prefer them.

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

This function provides the form elements to control the display

## Examples

``` r
colormakerInput("myid")
#> Error in colormakerInput("myid"): could not find function "colormakerInput"
```
