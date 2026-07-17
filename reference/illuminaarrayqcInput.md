# The input function of the illuminaarrayqc module

This module plots control probes from an illumina microarray experiment.

## Usage

``` r
illuminaarrayqcInput(id, eselist)
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

This funcion provides the form elements to control the display

## Examples

``` r
library(shinyngs)
illuminaarrayqcInput("myid", eselist)
#> Error in illuminaarrayqcInput("myid", eselist): could not find function "illuminaarrayqcInput"
```
