# The input function of the topgeneboxplot module

This module shows a faceted boxplot, one panel per gene, for the top
differential genes in a selected contrast (grouped by that contrast's
condition, with an optional beeswarm point overlay). Gene selection
reuses the
[`selectmatrix`](https://pinin4fjords.github.io/shinyngs/reference/selectmatrix.md)
and
[`contrasts`](https://pinin4fjords.github.io/shinyngs/reference/contrasts.md)
modules used elsewhere for expression/contrast selection, plus
[`colormaker`](https://pinin4fjords.github.io/shinyngs/reference/colormaker.md)
for palette choice.

## Usage

``` r
topgeneboxplotInput(id, eselist)
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

## Examples

``` r
topgeneboxplotInput("topgeneboxplot", eselist)
#> Error in topgeneboxplotInput("topgeneboxplot", eselist): could not find function "topgeneboxplotInput"
```
