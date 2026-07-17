# The server function of the genesetselect module

The gene set module is for adding a gene set filter to displays. A
[`selectizeInput`](https://rdrr.io/pkg/shiny/man/selectInput.html) is
used for performance reasons, providing an autocomplete field for
selecting from a list that could stretch to thousands of entries. This
would be difficult to do client-side using a standard select field.

## Usage

``` r
genesetselect(
  id,
  eselist,
  getExperiment,
  multiple = TRUE,
  filter_by_type = FALSE,
  require_select = TRUE
)
```

## Arguments

- id:

  Module namespace

- eselist:

  An ExploratorySummarizedExperimentList with its gene_sets slot set

- getExperiment:

  Accessor for returning an ExploratorySummarizedExperiment object, with
  'labelfield' set in its slots

- multiple:

  Boolean: should it be possible to select multiple gene sets?

- filter_by_type:

  Provide a filter for gene set type?

- require_select:

  Require a gene set selection?

## Value

output A list of two reactive functions which will be used by other
modules.

## Examples

``` r
geneset_functions <- genesetselect("heatmap", getExperiment)
#> Error in genesetselect("heatmap", getExperiment): could not find function "genesetselect"
```
