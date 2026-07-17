# The output function of the dexseqtable module

This module produces a differential exon usage table based on the output
`DEXSeqResults` object of the DEXSeq package.

## Usage

``` r
dexseqtableOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

For the table to be displayed, the `dexseq_results` slot must be filled
on at least one of the component `ExploratorySummarizedExperiment`
objects of the input `ExploratorySummarizedExperimentList`.

`dexseq_results` must be a list of `DEXSeqResults` objects corresponding
to the contrasts listed in the `contrasts` slot of the
`ExploratorySummarizedExperiment`.

Leverages the `simpletable` module

## Examples

``` r
dexseqtableOutput("experiment")
#> Error in dexseqtableOutput("experiment"): could not find function "dexseqtableOutput"
```
