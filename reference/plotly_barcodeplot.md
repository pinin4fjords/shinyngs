# Make an interactive gene set barcode plot with plotly

Reproduces the statistics drawn by
[`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html) (rank
ordering, quantile shading and the enrichment "worm" curve) as an
interactive `plotly` plot: rug ticks mark where gene set members fall in
the statistic ranking, and a curve on a secondary axis shows local
enrichment relative to the overall rate.

## Usage

``` r
plotly_barcodeplot(
  fold_changes,
  gene_ids,
  set_gene_ids,
  labels = gene_ids,
  plot_title = "",
  worm_span = 0.45
)
```

## Arguments

- fold_changes:

  A numeric vector of fold changes (or other ranking statistic), one per
  gene

- gene_ids:

  Gene IDs for the values in `fold_changes`, in the same ID space as
  `set_gene_ids`. Must be the same length and gene order as
  `fold_changes` and `labels`

- set_gene_ids:

  Gene IDs for the gene set

- labels:

  Display labels for the values in `fold_changes`, used in hover text
  (default: `gene_ids`). Must be the same length and gene order as
  `fold_changes` and `gene_ids`

- plot_title:

  A title for the plot

- worm_span:

  Span passed to the tricube moving average used for the enrichment
  curve (default: 0.45, matching limma's default)

## Value

output A plotly plot object
