# The server function of the rnaseq module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
rnaseq(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
rnaseq("rnaseq", eselist)
#> Error in rnaseq("rnaseq", eselist): could not find function "rnaseq"
```
