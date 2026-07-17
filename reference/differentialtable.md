# The server function of the differentialtable module

This module provides information on the comparison betwen pairs of
groups defined in a 'contrasts' slot of a
ExploratorySummarizedExperimentList

## Usage

``` r
differentialtable(id, eselist)
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

Essentially this funnction uses the `contrasts` module to group samples
and calculate fold changes, adding test statistics where available.

## Examples

``` r
differentialtable("differentialtable", eselist)
#> Error in differentialtable("differentialtable", eselist): could not find function "differentialtable"
```
