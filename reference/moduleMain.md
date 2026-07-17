# Assemble a module's main-panel content

Lays out a module's output: an optional help-modal trigger floated to
the top right, the section title, and the output body (plots, tables).
The content sits directly in the panel with no surrounding card, keeping
the plot area uncluttered. Pass `title = NULL` for modules that render
their own (often dynamic) heading, to avoid a duplicate title.

## Usage

``` r
moduleMain(title, ..., help = NULL)
```

## Arguments

- title:

  Section title (a string or tag), or `NULL` to omit it

- ...:

  Body content (plots, tables, sub-headings)

- help:

  Optional help-modal trigger, floated to the top right

## Value

A `tagList`
