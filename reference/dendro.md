# The server function of the dendrogram module

This module will produce a sample clustering dendrogram based on
user-selected parameters of row (e.g. gene) and column (sample)
selection provided by the `selectmatrix` module, as well distance matrix
generation and clustering method.

## Usage

``` r
dendro(id, eselist)
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
dendro("myid", eselist)
#> Error in dendro("myid", eselist): could not find function "dendro"
```
