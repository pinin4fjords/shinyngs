# Make a faceted boxplot of the top differential genes in a contrast

Draws one boxplot facet per gene, samples grouped by condition, with an
optional beeswarm overlay of individual points. This function only
renders: gene ranking, significance filtering and q values are computed
upstream by the caller (e.g. the
[`contrasts`](https://pinin4fjords.github.io/shinyngs/reference/contrasts.md)
module).

## Usage

``` r
static_topgene_boxplots(
  assay,
  groupby,
  genes,
  annotations = NULL,
  labels = NULL,
  beeswarm = TRUE,
  ncol = NULL,
  palette = NULL,
  palette_name = COLORBLIND_PALETTE_NAME,
  expressiontype = "expression",
  base_size = 11,
  should_transform = NULL
)
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

- annotations:

  Optional named character vector keyed by the values in `genes`,
  rendered as a per-facet annotation (e.g. a q value string)

- labels:

  Optional named character vector keyed by the values in `genes`, used
  as the facet title in place of the raw gene identifier (e.g. a gene
  symbol where `genes` holds Ensembl IDs). Genes missing from `labels`
  fall back to their raw identifier. `genes` itself still drives the
  lookup into `assay` and the matching of `annotations`.

- beeswarm:

  Overlay individual points using
  [`geom_quasirandom`](https://rdrr.io/pkg/ggbeeswarm/man/geom_quasirandom.html)?

- ncol:

  Number of facet columns. Defaults to `min(3, length(genes))`

- palette:

  Palette of colours, one for each unique value of `groupby`

- palette_name:

  Valid R color palette name

- expressiontype:

  Expression type for use in y axis label

- base_size:

  Passed to ggplot's `theme()`

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

## Value

output A `ggplot` output

## Examples

``` r
require(airway)
data(airway, package = "airway")
mat <- assays(airway)[[1]][1:4, ]
groupby <- as.character(colData(airway)$dex)
static_topgene_boxplots(mat, groupby, rownames(mat))

```
