# Shared UI input scaffolding for the differential-scatter plot modules

[`volcanoplot`](https://pinin4fjords.github.io/shinyngs/reference/volcanoplot.md),
[`maplot`](https://pinin4fjords.github.io/shinyngs/reference/maplot.md)
and
[`foldchangeplot`](https://pinin4fjords.github.io/shinyngs/reference/foldchangeplot.md)
are all scatter plots of statistics derived from a contrast in the
`contrasts` slot of an `ExploratorySummarizedExperimentList`, and share
the same input scaffolding (expression matrix selection, contrast
selection, scatter plot controls, gene highlighting, table export). Only
the numeric transform and threshold-line logic genuinely differ between
them - see
[`differentialscatterLogic`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterLogic.md).

## Usage

``` r
differentialscatterInput(
  id,
  eselist,
  scatter_id,
  require_contrast_stats = FALSE,
  multi_view_fn = function(esel) !singleValidMatrix(esel)
)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- scatter_id:

  Sub-namespace used for the scatterplot, gene highlighting and table
  export controls (e.g. `"volcano"`, `"ma"`, `"foldchange"`)

- require_contrast_stats:

  Restrict `eselist` to experiments with a populated `contrast_stats`
  slot before building controls

- multi_view_fn:

  Function of the (possibly filtered) `eselist` returning `TRUE` if the
  expression-matrix controls should be shown as their own fieldset
  rather than pushed in as hidden fields

## Value

A list of input tags
