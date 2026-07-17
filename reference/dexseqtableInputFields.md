# Make input fields for producing a table of differential exon usage. Separated here for re-use by the dexseqplot module

Make input fields for producing a table of differential exon usage.
Separated here for re-use by the dexseqplot module

## Usage

``` r
dexseqtableInputFields(id, eselist, allow_filtering = TRUE)
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

output Named list of `shiny.tag` objects
