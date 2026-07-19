# Shared server logic for the differential-scatter plot modules

Wires up the `selectmatrix`, `contrasts`, `geneselect`, `scatterplot`
and `simpletable` modules in the way common to
[`volcanoplot`](https://pinin4fjords.github.io/shinyngs/reference/volcanoplot.md),
[`maplot`](https://pinin4fjords.github.io/shinyngs/reference/maplot.md)
and
[`foldchangeplot`](https://pinin4fjords.github.io/shinyngs/reference/foldchangeplot.md).
The caller supplies `buildTable` to derive the module-specific numeric
matrix and `buildLines` to derive the module-specific threshold lines;
the colorby/label annotation (see
[`annotateDifferentialTable`](https://pinin4fjords.github.io/shinyngs/reference/annotateDifferentialTable.md))
and plot title are identical across all three and applied here.

## Usage

``` r
differentialscatterLogic(
  input,
  output,
  session,
  eselist,
  scatter_id,
  buildTable,
  buildLines,
  filename,
  require_contrast_stats = FALSE
)
```

## Arguments

- input, output, session:

  The Shiny input/output/session of the calling module

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- scatter_id:

  Sub-namespace matching the one passed to
  [`differentialscatterInput`](https://pinin4fjords.github.io/shinyngs/reference/differentialscatterInput.md)

- buildTable:

  Function of `contrast_reactives` returning the module-specific data
  frame, before colorby/label annotation. Its first two columns are used
  as the plotted x and y values.

- buildLines:

  Function of the annotated table and `contrast_reactives` returning the
  threshold-line data frame

- filename:

  Base filename used for the exported table

- require_contrast_stats:

  Restrict `eselist` to experiments with a populated `contrast_stats`
  slot

## Details

Must be called from within the caller's own `moduleServer()` - it does
not open its own namespace.
