# The input function of the dendrogram module

This module will produce a sample clustering dendrogram based on
user-selected parameters of row (e.g. gene) and column (sample)
selection provided by the `selectmatrix` module, as well distance matrix
generation and clustering method.

## Usage

``` r
dendroInput(id, eselist)
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

This funcion provides the form elements to control the display

## Examples

``` r
library(shinyngs)
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"
dendroInput("myid", eselist)
#> Error in dendroInput("myid", eselist): could not find function "dendroInput"
```
