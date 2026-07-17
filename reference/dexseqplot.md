# The server function of the dexseqplot Shiny module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
dexseqplot(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Details

This module produces a differential exon usage plot using the
`plotDEXSeq` function of the DEXSeq package.

For the plot to be displayed, the `dexseq_results` slot must be filled
on at least one of the component `ExploratorySummarizedExperiment`
objects of the input `ExploratorySummarizedExperimentList`.

`dexseq_results` must be a list of `DEXSeqResults` objects corresponding
to the contrasts listed in the `contrasts` slot of the
`ExploratorySummarizedExperiment`.

## Examples

``` r
dexseqplot("dexseqplot", eselist)
#> Error in dexseqplot("dexseqplot", eselist): could not find function "dexseqplot"
```
