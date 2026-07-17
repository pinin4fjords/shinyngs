# The input function of the `labelselectfield` module

This module provides an input which allows filtering on the basis of
data in the metadata slot of an `ExploratorySummarizedExperiment`. It
will only be called by other modules requiring that input.

## Usage

``` r
labelselectfieldInput(id, max_items = 1, id_selection = FALSE)
```

## Arguments

- id:

  Submodule namespace

- max_items:

  Maximum number of items that can be selected

- id_selection:

  Allow users to pick specific ID from those that relate to the
  specified label? (default: FALSE)

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

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
labelselectfieldInput("myid")
#> Error in labelselectfieldInput("myid"): could not find function "labelselectfieldInput"
```
