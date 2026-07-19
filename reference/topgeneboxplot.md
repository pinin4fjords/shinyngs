# The server function of the topgeneboxplot module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
topgeneboxplot(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
topgeneboxplot("topgeneboxplot", eselist)
#> Error in topgeneboxplot("topgeneboxplot", eselist): could not find function "topgeneboxplot"
```
