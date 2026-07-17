# The input function of the gene module

The gene module picks specified rows out the assay data, either simply
by id or label. This is used to create a gene-centric info page.

## Usage

``` r
geneInput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Inputs are a gene label and a variable to color by (where available)

## Examples

``` r
geneInput(ns("gene"), eselist)
#> Error in geneInput(ns("gene"), eselist): could not find function "geneInput"
```
