# Colour points by a chosen variable, with a plotly dropdown to switch it

Builds one set of per-level marker traces per entry of `colorby_menu`,
all but the first hidden initially, plus a plotly `updatemenus` dropdown
that toggles which set is visible. This is a standalone alternative to
the labelled/unselected trace pair built by
[`addPoints`](https://pinin4fjords.github.io/shinyngs/reference/addPoints.md),
used only when a caller wants readers to be able to recolour the plot
themselves (e.g. a static report); it does not support a
labelled/unselected point split.

## Usage

``` r
addColorbyMenu(
  x,
  y,
  z = NULL,
  colorby_menu,
  labels = NULL,
  plot_type = "scatter",
  point_size = 5,
  title = "",
  palette_name = COLORBLIND_PALETTE_NAME
)
```

## Arguments

- x:

  X coordinates

- y:

  Y coordinates

- z:

  Optional Z coordinates

- colorby_menu:

  Named list of string vectors/factors, one per dropdown option, each
  the same length as x/y(/z)

- labels:

  Optional hover labels, constant across dropdown options

- plot_type:

  Plot type: 'scatter' or 'scatter3d'

- point_size:

  Main point size

- title:

  Base plot title; each dropdown option appends "coloured by X"

- palette_name:

  Valid R color palette name, applied per option

## Value

output Plotly plot object
