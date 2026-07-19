# Reshape an assay matrix into long form for [`ggplot_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md)

Reshape an assay matrix into long form for
[`ggplot_topgene_boxplots`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_topgene_boxplots.md)

## Usage

``` r
topgeneBoxplotData(assay, groupby, genes, should_transform = NULL)
```

## Arguments

- assay:

  Numeric matrix, genes (rows) by samples (columns)

- groupby:

  Vector of group labels, one per column of `assay`

- genes:

  Character vector of row names of `assay` to facet on, in the order
  facets should appear. Also used to look up values in `assay`, so must
  match its row names even when `labels` is supplied.

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

## Value

A data frame with columns `gene` (factor, levels = `genes`), `group`
(factor, first-seen order) and `value`
