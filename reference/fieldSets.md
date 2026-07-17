# Create sets of fields for display

Shiny apps can get cluttered with many inputs. This method wraps sets of
fields in collapsible Bootstrap panels, one per named element, using
Bootstrap's own `data-toggle="collapse"` markup rather than a separate
collapsible-panel package. Every panel collapses/expands independently
(there is no "close others on open" behaviour).

## Usage

``` r
fieldSets(id, fieldset_list, open = NULL)
```

## Arguments

- id:

  ID field to apply to the overall container

- fieldset_list:

  A named list, each element containing one or more fields.

- open:

  Controls which panels are open by default, as a character vector of
  panel names. In most cases all should be left open (the default),
  since fields in collapsed panels may be less discoverable.

## Value

A `tagList` of Bootstrap panels
