# The server function of the geneselect module

This module provides controls for selecting genes (matrix rows) by
various criteria such as variance and gene set.

## Usage

``` r
geneselect(
  id,
  eselist,
  getExperiment,
  var_n = 50,
  var_max = 500,
  selectSamples = reactive({
     colnames(getExperiment())
 }),
  getAssay,
  provide_all = TRUE,
  provide_none = FALSE,
  default = NULL
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- getExperiment:

  Reactive expression which returns a ExploratorySummarizedExperiment
  object with assay and experimental data

- var_n:

  The number of rows to select when doing so by variance. Default = 50

- var_max:

  The maximum umber of rows to select when doing so by variance. Default
  = 500

- selectSamples:

  A reactive expression that provides a vector of samples to use, e.g.
  in row-wise variance calculation

- getAssay:

  A reactive expression providing the current assay selection.

- provide_all:

  Allow the 'all rows' selection in the UI? Means we don't have to
  calculate variance so the display is quicker, but it's a bad idea for
  e.g. heatmaps where the visual scales by the number of rows.

- provide_none:

  Allow the 'none' selection in the UI to make row selection optional.

- default:

  Default gene selection method

## Value

output A list of reactive functions for interrogating the selected rows.

## Examples

``` r
geneselect_functions <- geneselect("heatmap", getExperiments)
#> Error in geneselect("heatmap", getExperiments): could not find function "geneselect"
```
