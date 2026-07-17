# The UI function of the genesetselect module

The gene set module is for adding a gene set filter to displays. A
[`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html) is
used for performance reasons, providing an autocomplete field for
selecting from a list that could stretch to thousands of entries. This
would be difficult to do client-side using a standard select field.

## Usage

``` r
genesetselectInput(id, multiple = TRUE)
```

## Arguments

- id:

  Submodule namespace

- multiple:

  Boolean: should it be possible to select multiple gene sets?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
genesetselectInput("myid")
#> Error in genesetselectInput("myid"): could not find function "genesetselectInput"
```
