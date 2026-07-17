# The server function of the selectmatrix module

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
selectmatrix(
  id,
  eselist,
  var_n = 50,
  var_max = NULL,
  select_assays = TRUE,
  select_samples = TRUE,
  select_genes = TRUE,
  provide_all_genes = FALSE,
  default_gene_select = NULL,
  require_contrast_stats = FALSE,
  rounding = 2,
  select_meta = TRUE,
  allow_summarise = TRUE
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- var_n:

  The number of rows to select when doing so by variance. Default = 50

- var_max:

  The maximum umber of rows to select when doing so by variance. Default
  = 500

- select_assays:

  Provide UI and functions for assay selection?

- select_samples:

  Provide UI and functions for sample selection? (Default: TRUE)

- select_genes:

  Provide UI and functions for gene (row) selection? (Default: TRUE)

- provide_all_genes:

  Allow the 'all rows' selection in the UI? Means we don't have to
  calculate variance so the display is quicker, but it's a bad idea for
  e.g. heatmaps where the visual scales by the numbre of rows.

- default_gene_select:

  The default method to use for selecting genes

- require_contrast_stats:

  Only use elements of `eselist` that have a populated `contrast_stats`
  slot. For plots using p value data etc, this is used to hide
  experiments that don't have the necessary data.

- rounding:

  Number of decimal places to show in results (Default 2)

- select_meta:

  Boolean- add metadata controls?

- allow_summarise:

  Boolean, show controls for matrix summarisation?

## Value

output A list of reactive functions for fetching the derived matrix and
making a title based on its properties.

## Details

The
[`geneselect`](https://pinin4fjords.github.io/shinyngs/reference/geneselect.md)
and
[`sampleselect`](https://pinin4fjords.github.io/shinyngs/reference/sampleselect.md)
modules provide row- and column- selection, respectively. This will
generally not be called directly, but by other modules such as the
heatmap module.

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
selectSamples <- sampleselect("selectmatrix", eselist)
#> Error in sampleselect("selectmatrix", eselist): could not find function "sampleselect"
```
