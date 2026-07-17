# The server function of the groupby module

The groupby module provides a UI element to choose from the `group_vars`
in a SummarizedExperment. Useful for coloring in a PCA etc

## Usage

``` r
groupby(
  id,
  eselist,
  group_label = "Group by",
  multiple = FALSE,
  selectColData = NULL,
  isDynamic = reactive({
     TRUE
 })
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- group_label:

  A label for the grouping field

- multiple:

  Produces a checkbox group if true, a select box if false

- selectColData:

  Reactive returning an experiment matrix, probably derived from the
  [`selectmatrix`](https://pinin4fjords.github.io/shinyngs/reference/selectmatrix.md)
  module.

- isDynamic:

  Reactive expression providing a boolean. A FALSE value causes the
  groupby option to be placed in a hidden field.

## Value

output A list of reactive functions which will be used by other modules.

## Examples

``` r
geneset_functions <- groupby("heatmap", getExperiment)
#> Error in groupby("heatmap", getExperiment): could not find function "groupby"
```
