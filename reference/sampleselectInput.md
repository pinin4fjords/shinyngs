# The UI input function of the sampleselect module

This module provides controls for selecting matrix columns by sample or
group name.

## Usage

``` r
sampleselectInput(id, eselist, getExperiment, select_samples = TRUE)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- getExperiment:

  Reactive expression that returns a `ExploratorySummarizedExperiment`
  with assays and metadata. Usually a result of a user selection

- select_samples:

  Select samples at all? If set to false, a hidden input indicating the
  selection of all samples is produced.

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This will generally not be called directly, but by other modules such as
the selectmatrix module.

## Examples

``` r
sampleselectInput(ns("heatmap"))
#> Error in sampleselectInput(ns("heatmap")): could not find function "sampleselectInput"
```
