# Input function for scatterplotcontrols module

This module provides controls (2D/3D, axes etc) for scatter plots, which
may then be used by one or more instances of the scatterplot module.

## Usage

``` r
scatterplotcontrolsInput(id, allow_3d = TRUE, make_colors = FALSE)
```

## Arguments

- id:

  Submodule namespace

- allow_3d:

  Boolean: allow user to choose 3D plotting?

- make_colors:

  Boolean: add controls for coloring?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
scatterplotcontrolsInput("pca", allow_3d = FALSE) # for a 2D plot
#> Error in scatterplotcontrolsInput("pca", allow_3d = FALSE): could not find function "scatterplotcontrolsInput"
```
