# Build the named vector of facet titles used by [`ggplot_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md) and [`plotly_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/plotly_topgene_boxplots.md)

Genes with no entry in `labels` (or when `labels` is `NULL`) fall back
to their raw identifier, so callers always get one title per gene
regardless of annotation coverage.

## Usage

``` r
topgeneFacetLabels(labels, genes)
```

## Arguments

- labels:

  Optional named character vector keyed by the values in `genes`, used
  as the facet title in place of the raw gene identifier (e.g. a gene
  symbol where `genes` holds Ensembl IDs). Genes missing from `labels`
  fall back to their raw identifier. `genes` itself still drives the
  lookup into `assay` and the matching of `annotations`.

- genes:

  Character vector of row names of `assay` to facet on, in the order
  facets should appear. Also used to look up values in `assay`, so must
  match its row names even when `labels` is supplied.

## Value

A character vector parallel to `genes`, named by `genes`
