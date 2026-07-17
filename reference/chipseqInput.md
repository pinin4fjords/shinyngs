# The input function of the chipseq module. Currently a near-clone of the RNA-seq module, with ChIP-seq optimisations planned.

This provides the form elements to control the ChIP-seq display

## Usage

``` r
chipseqInput(id, eselist)
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

The chipseq module is a combination of output from many modules (pca,
boxplot etc) to form a comprehensive analysis application.

## Examples

``` r
chipseqInput("chipseq", eselist)
#> Error in chipseqInput("chipseq", eselist): could not find function "chipseqInput"
```
