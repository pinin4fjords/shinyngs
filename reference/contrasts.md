# The server function of the contrasts module

This module provides the form elements to control contrasts used in e.g.
differential expression panels. In particular it provides the ability
for users to add filters to progressively refine a query.

## Usage

``` r
contrasts(
  id,
  eselist,
  selectmatrix_reactives = list(),
  multiple = FALSE,
  select_all_contrasts = FALSE,
  show_controls = TRUE,
  default_foldchange = 2,
  default_pval = 0.05,
  default_qval = 0.1
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- selectmatrix_reactives:

  The list of reactive expressions returned by a call to the
  [`selectmatrix`](https://pinin4fjords.github.io/shinyngs/reference/selectmatrix.md)
  module. This will be unpacked to gain access to the data provided by
  those reaactive.

- multiple:

  Allow selection of multiple contrasts?

- select_all_contrasts:

  Select all contrasts by default?

- show_controls:

  Show the controls for contrast selection?

- default_foldchange:

  default value for the fold change filter

- default_pval:

  Default value for the p value field

- default_qval:

  Default value for the q value field

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Examples

``` r
contrasts("differential", getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)
#> Error in contrasts("differential", getExperiment = getExperiment, selectMatrix = selectMatrix,     getAssay = getAssay, multiple = TRUE): unused arguments (getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)
```
