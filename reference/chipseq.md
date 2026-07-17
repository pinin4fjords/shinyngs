# The server function of the chipseq module. Currently a near-clone of the RNA-seq module, with ChIP-seq optimisations planned.

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
chipseq(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
chipseq("chipseq", eselist)
#> Error in chipseq("chipseq", eselist): could not find function "chipseq"
```
