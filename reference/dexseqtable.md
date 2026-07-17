# The server function of the dexseqtable module

This module produces a differential exon usage table based on the output
`DEXSeqResults` object of the DEXSeq package.

## Usage

``` r
dexseqtable(
  id,
  eselist,
  allow_filtering = TRUE,
  getDEUGeneID = NULL,
  show_controls = TRUE,
  page_length = 15,
  link_to_deu_plot = TRUE
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- allow_filtering:

  Allow user filtering of results (default: TRUE)? Deactivated by the
  `dexseqplot` module, which uses it for showing gene-specific results.

- getDEUGeneID:

  Reactive expression returning a gene ID.

- show_controls:

  Passed to
  [`simpletable`](https://pinin4fjords.github.io/shinyngs/reference/simpletable.md),
  spcifies whether the various `datatables` controls are displayed
  (default: TRUE).

- page_length:

  Passed to
  [`simpletable`](https://pinin4fjords.github.io/shinyngs/reference/simpletable.md),
  spcifies the number of rows to display (default: 15).

- link_to_deu_plot:

  Link label fields to the plots produced by
  [`dexseqplot`](https://pinin4fjords.github.io/shinyngs/reference/dexseqplot.md)?
  (default: TRUE)

## Details

For the table to be displayed, the `dexseq_results` slot must be filled
on at least one of the component `ExploratorySummarizedExperiment`
objects of the input `ExploratorySummarizedExperimentList`.

`dexseq_results` must be a list of `DEXSeqResults` objects corresponding
to the contrasts listed in the `contrasts` slot of the
`ExploratorySummarizedExperiment`.

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
dexseqtable("dexseqtable", eselist)
#> Error in dexseqtable("dexseqtable", eselist): could not find function "dexseqtable"
```
