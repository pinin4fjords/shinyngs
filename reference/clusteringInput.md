# The input function of the clustering module

This module plots the expression profiles (scaled for comparison) of the
selected rows of the input matrix provided by the `selectmatrix` module.

## Usage

``` r
clusteringInput(id, eselist)
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

The `clara` method, a fast approximation of partitioning about medoids,
is used to produce the clusters. As well as defining the input matrix
users can decide how the clusters are drawn and how many clusters should
be generated.

This funcion provides the form elements to control the display

## Examples

``` r
library(shinyngs)
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> Creating ExploratorySummarizedExperimentList object
clusteringInput("myid", eselist)
#> Error in clusteringInput("myid", eselist): could not find function "clusteringInput"
```
