# The input function for the `modal` module

This module uses Shiny's `modalDialog()` to create overlaid text for the
current panel which displays when a link is clicked. The link is placed
in the output function of the calling module, and the modal is shown by
[`modalServer()`](https://pinin4fjords.github.io/shinyngs/reference/modalServer.md)
in the calling module's server function (see example).

## Usage

``` r
modalInput(id, label, class, icon = "info-circle")
```

## Arguments

- id:

  Modal ID. Must match that passed to `modalServer`

- label:

  A label to use for the link

- class:

  A class to apply to the link

- icon:

  Icon used to activate modal

## Details

This is handy, for example when adding help text.

## Examples

``` r
modalInput(ns("dendro"), "help")
#> Error in modalInput(ns("dendro"), "help"): could not find function "modalInput"
```
