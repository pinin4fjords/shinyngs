# The output function of the differentialtable module

This module provides information on the comparison betwen pairs of
groups defined in a 'contrasts' slot of a
ExploratorySummarizedExperimentList

## Usage

``` r
differentialtableOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Leverages the `simpletable` module

## Examples

``` r
differentialtableOutput("experiment")
#> Error in differentialtableOutput("experiment"): could not find function "differentialtableOutput"
```
