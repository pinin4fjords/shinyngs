# The input function of the gene plotdownload module

The plotdownload module provides export functionality for panels with
plots. This will generally not be called directly, but by other modules

## Usage

``` r
plotdownloadInput(id, label = "Plot")
```

## Arguments

- id:

  Submodule namespace

- label:

  Label for the download button

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
plotdownloadInput(ns("heatmap"))
#> Error in plotdownloadInput(ns("heatmap")): could not find function "plotdownloadInput"
```
