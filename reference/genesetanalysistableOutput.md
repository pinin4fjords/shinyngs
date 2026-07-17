# The output function of the genesetanalysistable module

This module displays gene set analysis tables stored as a list in the
`gene_set_analyses` slot of an `ExploratorySummarizedExperiment`.

## Usage

``` r
genesetanalysistableOutput(id)
```

## Arguments

- id:

  Module namespace

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

The `gene_set_analyses` slot must be keyed first by the name of the
assay to which it pertains, and second by the gene set type (e.g.
'KEGG'). The containing `ExploratorySummarizedExperiment` must have a
populated `gene_sets` slot, keyed first by metadata column used to
define the gene sets and secondly by the gene set type.

The module is based on the output of roast() from `limma`, but it's
fairly generic, and assumes only the presence of a 'p value' and 'FDR'
column, so the output of other methods should be easily adapted to suit.

Leverages the `simpletable` module

## Examples

``` r

# The module output function is called like:

genesetanalysistableOutput("experiment")
#> Error in genesetanalysistableOutput("experiment"): could not find function "genesetanalysistableOutput"
```
