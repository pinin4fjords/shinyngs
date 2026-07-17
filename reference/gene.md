# The server function of the gene module

The gene module picks specified rows out the assay data, either simply
by id or label. This is used to create a gene-centric info page.

## Usage

``` r
gene(id, eselist)
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
gene("gene", eselist)
#> Error in gene("gene", eselist): could not find function "gene"
```
