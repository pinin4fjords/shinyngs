# Annotate a differential-scatter table with colorby and label columns

Shared tail of the `volcanoTable`/`maTable`/`foldchangeTable` reactives:
marks rows as hidden, matching the contrast filters, or in the
highlighted gene set, and attaches display labels for the highlighted
rows.

## Usage

``` r
annotateDifferentialTable(
  ct,
  contrast_reactives,
  geneselect_reactives,
  selectmatrix_reactives
)
```

## Arguments

- ct:

  Data frame keyed by feature id, as produced by the module-specific
  `buildTable` function

- contrast_reactives:

  Reactives returned by the `contrasts` module

- geneselect_reactives:

  Reactives returned by the `geneselect` module

- selectmatrix_reactives:

  Reactives returned by the `selectmatrix` module

## Value

The input data frame with `colorby` and `label` columns added
