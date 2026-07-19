# Is there only one matrix to plot from this object?

Convenience function for deciding how to construct filters

## Usage

``` r
singleValidMatrix(eselist)
```

## Arguments

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

## Value

output Logical value

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese, title = "Airway study")
#> Creating ExploratorySummarizedExperimentList object
singleValidMatrix(eselist)
#> [1] TRUE
```
