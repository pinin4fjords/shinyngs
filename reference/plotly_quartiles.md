# Make a line-based alternative to boxplots

Box-plots become unmanagable with large numbers of samples. This
function plots lines at the median, quartiles, and whiskers, plotting
points for outliers beyond that

## Usage

``` r
plotly_quartiles(
  matrix,
  labels = rownames(matrix),
  expressiontype = "expression",
  whisker_distance = 1.5,
  should_transform = NULL
)
```

## Arguments

- matrix:

  Numeric matrix

- labels:

  String vector of labels to be used for each matrix row

- expressiontype:

  Y axis label

- whisker_distance:

  IQR multiplier for whiskers, and beyond which to show outliers (see
  `coef` in
  [`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html))

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

## Examples

``` r
data(airway, package = "airway")
plotly_quartiles(assays(airway)[[1]], as(airway, "ExploratorySummarizedExperiment"))
#> Error in as.vector(x): no method for coercing this S4 class to a vector
```
