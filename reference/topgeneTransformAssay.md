# Log2-transform an assay matrix and subset/order it to the requested genes

Zeros are mapped to log2(1) = 0 rather than dropped to NA (the default
`rmzeros = FALSE`): unlike a per-sample boxplot, where dropping a
handful of zero values out of thousands of genes is harmless, a gene can
legitimately be all-zero in one whole condition - exactly the genes an
absolute-fold-change ranking surfaces - and dropping those to NA would
erase that side's box entirely instead of showing it pinned at zero.

## Usage

``` r
topgeneTransformAssay(assay, genes, should_transform = NULL)
```

## Arguments

- assay:

  Numeric matrix, genes (rows) by samples (columns)

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

The transformed matrix, subset to `genes` in that order
