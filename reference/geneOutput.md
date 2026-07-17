# The input function of the gene module

The gene module picks specified rows out the assay data, either simply
by id or label. This is used to create a gene-centric info page.

## Usage

``` r
geneOutput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  List of structuredExperiment objects with assay and experimental data

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

Outputs are a bar plot and a table contrast data for this gene

## Examples

``` r
geneOutput("gene", eselist)
#> Error in geneOutput("gene", eselist): could not find function "geneOutput"
```
