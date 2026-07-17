# The UI input function of the dexseqplot Shiny module

This module produces a differential exon usage plot using the
`plotDEXSeq` function of the DEXSeq package.

## Usage

``` r
dexseqplotInput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

For the plot to be displayed, the `dexseq_results` slot must be filled
on at least one of the component `ExploratorySummarizedExperiment`
objects of the input `ExploratorySummarizedExperimentList`.

`dexseq_results` must be a list of `DEXSeqResults` objects corresponding
to the contrasts listed in the `contrasts` slot of the
`ExploratorySummarizedExperiment`.

## Examples

``` r
dexseqplotInput("experiment", eselist)
#> Error in dexseqplotInput("experiment", eselist): could not find function "dexseqplotInput"
```
