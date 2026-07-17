# The output function of the contrasts module

This module provides the form elements to control contrasts used in e.g.
differential expression panels. In particular it provides the ability
for users to add filters to progressively refine a query.

## Usage

``` r
contrastsOutput(id)
```

## Arguments

- id:

  Submodule namespace

## Details

This function provides a summary. Actual output should be rendered by
calling modules using the provided reactives.

## Examples

``` r
contrastsOutput("myid")
#> Error in contrastsOutput("myid"): could not find function "contrastsOutput"
```
