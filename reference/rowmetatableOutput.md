# The output function of the rowmetatable module

This module produces a simple table of the
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
in an ExploratorySummarizedExperiment object. If more than one of these
objects were specified, a select box should appear to pick the desired
one for display.

## Usage

``` r
rowmetatableOutput(id)
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
rowmetatableOutput("experiment")
#> Error in rowmetatableOutput("experiment"): could not find function "rowmetatableOutput"
```
