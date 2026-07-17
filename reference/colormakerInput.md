# The input function of the colorby module

shinyngs colours every plot from the single, fixed colour-blind-safe
palette in
[`makeColorScale`](https://pinin4fjords.github.io/shinyngs/reference/makeColorScale.md),
so there is no per-plot palette choice to expose. Modules that build
their form from a list of inputs can include this call unconditionally;
it contributes no UI element.

## Usage

``` r
colormakerInput(id)
```

## Arguments

- id:

  Submodule namespace

## Value

output NULL

## Examples

``` r
colormakerInput("myid")
#> Error in colormakerInput("myid"): could not find function "colormakerInput"
```
