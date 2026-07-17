# The server function of the illuminaarrayqc module

This module plots control probes from an illumina microarray experiment.

## Usage

``` r
illuminaarrayqc(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
illuminaarayqc("myid", eselist)
#> Error in illuminaarayqc("myid", eselist): could not find function "illuminaarayqc"
```
