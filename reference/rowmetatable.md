# The server function of the rowmetatable module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).
Essentially this just passes the results of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
applied to the specified SummarizedExperiment object to the
`simpletable` module

## Usage

``` r
rowmetatable(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Examples

``` r
rowmetatable("rowmetatable", eselist)
#> Error in rowmetatable("rowmetatable", eselist): could not find function "rowmetatable"
```
