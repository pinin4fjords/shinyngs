# The output function of the dendro module

This module will produce a sample clustering dendrogram based on
user-selected parameters of row (e.g. gene) and column (sample)
selection provided by the `selectmatrix` module, as well distance matrix
generation and clustering method.

## Usage

``` r
dendroOutput(id)
```

## Arguments

- id:

  Submodule namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This provides actual dendrogram plot element for display by applications

## Examples

``` r
dendroOutput("myid")
#> Error in dendroOutput("myid"): could not find function "dendroOutput"
```
