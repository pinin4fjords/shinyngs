# Build the per-facet annotation data frame used by [`static_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/static_topgene_boxplots.md)

Build the per-facet annotation data frame used by
[`static_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/static_topgene_boxplots.md)

## Usage

``` r
topgeneAnnotationData(annotations, genes)
```

## Arguments

- annotations:

  Optional named character vector keyed by the values in `genes`,
  rendered as a per-facet annotation (e.g. a q value string)

- genes:

  Character vector of row names of `assay` to facet on, in the order
  facets should appear. Also used to look up values in `assay`, so must
  match its row names even when `labels` is supplied.

## Value

A data frame with columns `gene` (factor, levels = `genes`) and `label`,
or `NULL` if no annotations were supplied/matched
