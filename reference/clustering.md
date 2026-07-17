# The server function of the clustering module

This module plots the expression profiles (scaled for comparison) of the
selected rows of the input matrix provided by the `selectmatrix` module.

## Usage

``` r
clustering(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Details

The `clara` method, a fast approximation of partitioning about medoids,
is used to produce the clusters. As well as defining the input matrix
users can decide how the clusters are drawn and how many clusters should
be generated.

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
clustering("myid", eselist)
#> Error in clustering("myid", eselist): could not find function "clustering"
```
