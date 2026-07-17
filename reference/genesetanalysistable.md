# The server function of the genesetanalysistable module

This module displays gene set analysis tables stored as a list in the
`gene_set_analyses` slot of an `ExploratorySummarizedExperiment`.

## Usage

``` r
genesetanalysistable(id, eselist)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Details

The `gene_set_analyses` slot must be keyed first by the name of the
assay to which it pertains, and second by the gene set type (e.g.
'KEGG'). The containing `ExploratorySummarizedExperiment` must have a
populated `gene_sets` slot, keyed first by metadata column used to
define the gene sets and secondly by the gene set type.

The module is based on the output of roast() from `limma`, but it's
fairly generic, and assumes only the presence of a 'p value' and 'FDR'
column, so the output of other methods should be easily adapted to suit.

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).
Essentially this just passes the results of
[`colData()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
applied to the specified SummarizedExperiment object to the
`simpletable` module

## Examples

``` r

data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"

# This module needs an eselist whose experiments carry gene_set_analyses
# results (see the vignette). Given those, the module server is called like:

if (interactive()) {
  genesetanalysistable("genesetanalysistable", eselist)
}
```
