# The UI input function of the dexseqtable module

This module produces a differential exon usage table based on the output
`DEXSeqResults` object of the DEXSeq package.

## Usage

``` r
dexseqtableInput(id, eselist, allow_filtering = TRUE)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- allow_filtering:

  Allow user filtering of results (default: TRUE)? Deactivated by the
  `dexseqplot` module, which uses it for showing gene-specific results.

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
dexseqtableInput("experiment", eselist)
#> Error in dexseqtableInput("experiment", eselist): could not find function "dexseqtableInput"
```
