# The server function of the sampleselect module

This module provides controls for selecting matrix columns by sample or
group name.

## Usage

``` r
sampleselect(id, eselist, getExperiment, allow_summarise = TRUE)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- getExperiment:

  Reactive expression that returns a `ExploratorySummarizedExperiment`
  with assays and metadata. Usually a result of a user selection

- allow_summarise:

  Boolean, show controls for matrix summarisation?

## Value

output A list of reactive functions for interrogating the selected
samples/ columns.

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
selectSamples <- sampleselect("selectmatrix", getExperiment)
#> Error in sampleselect("selectmatrix", getExperiment): could not find function "sampleselect"
```
