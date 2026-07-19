# Compute the total plot height and inter-row margin fraction needed to lay out `n_genes` faceted boxplots over `ncol` columns without rows overlapping

Compute the total plot height and inter-row margin fraction needed to
lay out `n_genes` faceted boxplots over `ncol` columns without rows
overlapping

## Usage

``` r
topgeneBoxplotLayout(n_genes, ncol)
```

## Arguments

- n_genes:

  Number of gene facets being drawn

- ncol:

  Number of facet columns

## Value

A list with `nrows`, `height` (total plot height in pixels) and
`vertical_margin` (top/bottom margin fraction to pass to
[`subplot`](https://rdrr.io/pkg/plotly/man/subplot.html) so the gap
between rows is `topgeneboxplot_gap_px` regardless of `height`)
