# Make a boxplot with coloring by experimental variable

A simple function using `ggplot2` to make a sample boxplot

## Usage

``` r
static_boxplot(
  plotmatrices,
  experiment,
  colorby = NULL,
  palette = NULL,
  expressiontype = "expression",
  whisker_distance = 1.5,
  base_size = 11,
  palette_name = COLORBLIND_PALETTE_NAME,
  annotate_samples = FALSE,
  should_transform = NULL
)
```

## Arguments

- plotmatrices:

  Expression/ other data matrix, or named list thereof

- experiment:

  Annotation for the columns of plotmatrix

- colorby:

  Column name in `experiment` specifying how boxes should be colored

- palette:

  Palette of colors, one for each unique value derived from `colorby`.

- expressiontype:

  Expression type for use in y axis label

- whisker_distance:

  Passed to
  [`geom_boxplot`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)
  as `coef`, controlling the length of the whiskers. See documentation
  of that function for more info (default: 1.5).

- base_size:

  Passed to ggplot's `theme()`

- palette_name:

  Valid R color palette name

- annotate_samples:

  Add a suffix to sample labels reflecting their group?

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL, a conditional transformation based
  on threshold is applied.

## Value

output A `ggplot` output

## Examples

``` r
require(airway)
data(airway, package = "airway")
static_boxplot(assays(airway)[[1]], data.frame(colData(airway)), colorby = "dex")
```
