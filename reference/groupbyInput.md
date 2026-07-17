# The UI function of the groupby module

The groupby module provides a UI element to choose from the `group_vars`
in a SummarizedExperment. Useful for coloring in a PCA etc

## Usage

``` r
groupbyInput(id, color = TRUE)
```

## Arguments

- id:

  Submodule namespace

- color:

  Provide coloring functionality for groups?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()

## Examples

``` r
groupbyInput(ns("heatmap"))
#> Error in groupbyInput(ns("heatmap")): could not find function "groupbyInput"
```
