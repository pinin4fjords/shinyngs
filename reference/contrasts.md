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

The reactive graph is built by a sequence of internal factory functions,
each owning one cohesive stage (contrast enumeration, naming, the
dynamic filter-set engine, table building, selection, filtering,
labelling, and the query summary) - see `contrastEnumeration`,
`contrastNaming`, `contrastFilterSetEngine`, `contrastTableBuilder`,
`contrastSelection`, `contrastFiltering`, `contrastLabelling` and
`contrastQuerySummary`. This function itself just wires those stages
together in dependency order and returns their public reactives.

## Examples

``` r
contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = TRUE)
#> Error in contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives,     multiple = TRUE): unused arguments (eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = TRUE)
```
