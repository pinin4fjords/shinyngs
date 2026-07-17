# Make a complete set of filters for a contrast: the contrast itself, fold change, and where applicable p- and q- values.

Make a complete set of filters for a contrast: the contrast itself, fold
change, and where applicable p- and q- values.

## Usage

``` r
makeContrastFilterSet(
  ns,
  ese,
  assay,
  contrasts,
  contrast_numbers,
  multiple,
  show_controls,
  default_foldchange = 2,
  default_pval = 0.05,
  default_qval = 0.1,
  index = "",
  filter_rows = TRUE,
  select_all_contrasts = FALSE
)
```

## Arguments

- ns:

  A namespace function (created with
  [`NS`](https://rdrr.io/pkg/shiny/man/NS.html) to be used in creating
  field IDs.

- ese:

  ExploratorySummarizedExperiment object

- assay:

  Assay in `ese`

- contrasts:

  A list of lists specifying contrasts.

- contrast_numbers:

  A named vector of indices corresponding to `contrasts`.

- multiple:

  Allow multiple contrasts to be selected? Passed to
  [`makeContrastControl`](https://pinin4fjords.github.io/shinyngs/reference/makeContrastControl.md).

- show_controls:

  Show controls? Setting to false will cause them to be hidden.

- default_foldchange:

  Default value for the fold change field

- default_pval:

  Default value for the p value field

- default_qval:

  Default value for the q value field

- index:

  Index. Will be used to differentiate mutiple copies of the field set.

- filter_rows:

  Use fold change and p value etc to filter values?

- select_all_contrasts:

  Select all contrasts by default?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()
