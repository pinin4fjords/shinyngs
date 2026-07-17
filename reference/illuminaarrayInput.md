# The input function of the illuminaarray module

This provides the form elements to control the RNA-seq display

## Usage

``` r
illuminaarrayInput(id, eselist)
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

The illuminaarray module is a combination of output from many modules
(pca, boxplot etc) to form a comprehensive analysis application.

## Examples

``` r
illuminaarrayInput("illuminaarray", eselist)
#> Error in illuminaarrayInput("illuminaarray", eselist): could not find function "illuminaarrayInput"
```
