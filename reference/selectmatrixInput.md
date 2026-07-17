# The UI input function of the selectmarix module

This module forms the core of many operations in `shinyngs`. To use the
matrix data stored in an `ExploratorySummarizedExperimentList` object,
selection must be made on the basis of experiment (which
`ExploratorySummarizedExperiment`) to use), the specific assay of the
experiment, and the rows and columns of the selected assay matrix. This
module provides customisable controls for selection at each of these
levels, and parses those inputs to produce matrices used in the various
plots.

## Usage

``` r
selectmatrixInput(id, eselist, require_contrast_stats = FALSE)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- require_contrast_stats:

  Only use elements of `eselist` that have a populated `contrast_stats`
  slot. For plots using p value data etc, this is used to hide
  experiments that don't have the necessary data.

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

The
[`geneselect`](https://pinin4fjords.github.io/shinyngs/reference/geneselect.md)
and
[`sampleselect`](https://pinin4fjords.github.io/shinyngs/reference/sampleselect.md)
modules provide row- and column- selection, respectively. This will
generally not be called directly, but by other modules such as the
heatmap module.

## Examples

``` r
selectmatrixInput(ns("heatmap"), eselist)
#> Error in selectmatrixInput(ns("heatmap"), eselist): could not find function "selectmatrixInput"
```
