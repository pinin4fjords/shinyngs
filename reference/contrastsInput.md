# The input function of the contrasts module

This module provides the form elements to control contrasts used in e.g.
differential expression panels. In particular it provides the ability
for users to add filters to progressively refine a query.

## Usage

``` r
contrastsInput(
  id,
  allow_filtering = TRUE,
  summarise = TRUE,
  dynamic_filters = FALSE,
  select_summary_type = FALSE
)
```

## Arguments

- id:

  Submodule namespace

- allow_filtering:

  Provide the filtering fields? Can be disabled to produce unfiltered
  contrasts tables.

- summarise:

  Provide summarisation controls? Allow user to control how how values
  are summarised per group. Disabling this disables summarisation, which
  may be the desired result for modules that just need to use the
  contrasts drop-down.

- dynamic_filters:

  Logical indicating whether the user should be able to add progressive
  filters.

- select_summary_type:

  Allow user to select summary type (e.g. mean)? Passed to
  [`summarisematrixInput`](https://pinin4fjords.github.io/shinyngs/reference/summarisematrixInput.md).

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
contrastsInput("test")
#> Error in contrastsInput("test"): could not find function "contrastsInput"
```
