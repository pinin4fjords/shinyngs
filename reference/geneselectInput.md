# The UI input function of the geneselect module

This module provides controls for selecting genes (matrix rows) by
various criteria such as variance and gene set.

## Usage

``` r
geneselectInput(id, select_genes = TRUE)
```

## Arguments

- id:

  Submodule namespace

- select_genes:

  Disable gene (row) - wise selection if set to FALSE

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

This will generally not be called directly, but by other modules such as
the heatmap module.

## Examples

``` r
geneselectInput("myid")
#> Error in geneselectInput("myid"): could not find function "geneselectInput"
```
