# The UI input function of the differentialtable module

This module provides information on the comparison betwen pairs of
groups defined in a 'contrasts' slot of a
ExploratorySummarizedExperimentList

## Usage

``` r
differentialtableInput(id, eselist)
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

Leverages the `simpletable` module

## Examples

``` r
differentialtableInput("experiment", eselist)
#> Error in differentialtableInput("experiment", eselist): could not find function "differentialtableInput"
```
