# The server function of the illuminaarray module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
illuminaarray(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
illuminaarray("illuminaarray", eselist)
#> Error in illuminaarray("illuminaarray", eselist): could not find function "illuminaarray"
```
