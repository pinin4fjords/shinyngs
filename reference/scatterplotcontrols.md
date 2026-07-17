# Server function for scatterplotcontrols module

This module provides controls (2D/3D, axes etc) for scatter plots, which
may then be used by one or more instances of the scatterplot module.

## Usage

``` r
scatterplotcontrols(
  id,
  getDatamatrix,
  x = NA,
  y = NA,
  z = NA,
  makeColors = NULL
)
```

## Arguments

- id:

  Module namespace

- getDatamatrix:

  Reactive expression that returns a matrix from which coulumn headers
  will be used to create axis select drop-downs. The same reactive
  should be supplied to the scatterplot module

- x:

  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA)

- y:

  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA)

- z:

  A value supplied for this parameter will cause a hidden field to be
  generated instead of a select, useful for scatter plots that don't
  need the user to select axes (default: NA)

- makeColors:

  Boolean: use controls for coloring?

## Value

output A list of reactives for accessing input values

## Examples

``` r
unpack.list(scatterplotcontrols("pca", pcaMatrix, x = 1, y = 2)) # To have fixed axes rather than user-selected
#> Error in scatterplotcontrols("pca", pcaMatrix, x = 1, y = 2): could not find function "scatterplotcontrols"
```
