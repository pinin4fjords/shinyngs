# Shared output scaffolding for the differential-scatter plot modules

Shared output scaffolding for the differential-scatter plot modules

## Usage

``` r
differentialscatterOutput(id, scatter_id, title, modal)
```

## Arguments

- id:

  Module namespace

- scatter_id:

  Sub-namespace matching the one passed to
  [`differentialscatterInput`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterInput.md)

- title:

  Plot title shown above the output

- modal:

  A list with `id` and `title` elements identifying the help modal for
  this module (e.g. `volcanoplot_modal`)

## Value

output An HTML tag object that can be rendered as HTML using
as.character()
