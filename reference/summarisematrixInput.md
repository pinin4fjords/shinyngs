# The input function of the summarizematrix module

This module provides a form element and associated get function for
defining how a summary statistic is calculated (probably by mean).

## Usage

``` r
summarisematrixInput(id, allow_none = TRUE, select_summary_type = TRUE)
```

## Arguments

- id:

  Submodule namespace

- allow_none:

  Allow a 'no summarisation' selection.

- select_summary_type:

  Allow user to select summary type (e.g. mean)?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
contrastsInput("test")
#> Error in contrastsInput("test"): could not find function "contrastsInput"
```
