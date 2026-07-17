# The server function of the `labelselectfield` module

This module provides an input which allows filtering on the basis of
data in the metadata slot of an `ExploratorySummarizedExperiment`. It
will only be called by other modules requiring that input.

## Usage

``` r
labelselectfield(
  id,
  eselist,
  getExperiment = NULL,
  labels_from_all_experiments = FALSE,
  url_field = "label",
  max_items = 1,
  field_selection = FALSE,
  id_selection = FALSE,
  getNonEmptyRows = NULL,
  list_input = FALSE
)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- getExperiment:

  Reactive supplying an `ExploratorySummarizedExperiment`

- labels_from_all_experiments:

  Derive valid labels from all experiments?

- url_field:

  Parameter to extract from the URL to set the value of the select field

- max_items:

  Maximum number of metadata values that can be selected

- field_selection:

  Allow selection of the meta field to use (TRUE), or use labels (or row
  ids if the label field is not set)?

- id_selection:

  Allow users to pick specific ID from those that relate to the
  specified label? (default: FALSE)

- getNonEmptyRows:

  Optional reactive providing non empty matrix rows

- list_input:

  Boolean: will input be a list of values?

## Details

Where metadata is present, the user can select which field to select on,
and the value of that field (populated conditionall on field selction).
If specified, checkboxes are provided to allow selection of specific row
IDs.

A [`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html) is
used for performance reasons, providing an autocomplete field for
selecting from a list that could stretch to thousands of entries. This
would be difficult to do client-side using a standard select field.

## Examples

``` r
labelselectfield("myid", eselist)
#> Error in labelselectfield("myid", eselist): could not find function "labelselectfield"
```
