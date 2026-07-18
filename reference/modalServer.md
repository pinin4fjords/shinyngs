# The server function of the `modal` module

This module uses Shiny's `modalDialog()` to create overlaid text for the
current panel which displays when the link produced by
[`modalInput()`](https://pinin4fjords.github.io/shinyngs/reference/modalInput.md)
is clicked. It is called from the server function of the calling module,
using the same id passed to `modalInput` (see example).

## Usage

``` r
modalServer(id, title, content = NULL)
```

## Arguments

- id:

  Modal ID. Must match that passed to `modalInput`

- title:

  Title to show on the help modal. May be a function, which is called
  each time the modal opens, so titles that depend on reactive state
  stay current.

- content:

  Content to include in the modal. May be a function, called each time
  the modal opens. When `NULL` (the default), Markdown is loaded from
  `inst/inlinehelp/<id>.md` the first time the modal is opened, then
  cached for the life of the session.

## Details

This is handy, for example when adding help text.

## Examples

``` r
modalServer("dendro", "Sample clustering dendrogram")
#> Error in modalServer("dendro", "Sample clustering dendrogram"): could not find function "modalServer"
```
