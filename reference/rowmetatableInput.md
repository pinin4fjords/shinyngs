# The UI input function of the rowmetatable module

This module produces a simple table of the row metadata (accessed via
`mcols`) in a SummarizedExperiment object. If more than one of these
objects were specified, a select box should appear to pick the desired
one for display.

## Usage

``` r
rowmetatableInput(id, eselist)
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
experimentableInput("experiment", eselist)
#> Error in experimentableInput("experiment", eselist): could not find function "experimentableInput"
```
