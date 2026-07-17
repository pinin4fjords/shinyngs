# The input function of the rnaseq module

This provides the form elements to control the RNA-seq display

## Usage

``` r
rnaseqInput(id, eselist)
```

## Arguments

- id:

  Submodule namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Details

The rnaseq module is a combination of output from many modules (pca,
boxplot etc) to form a comprehensive analysis application.

## Examples

``` r
rnaseqInput("rnaseq", eselist)
#> Error in rnaseqInput("rnaseq", eselist): could not find function "rnaseqInput"
```
