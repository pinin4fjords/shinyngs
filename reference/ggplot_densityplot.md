# Make a static density plot with ggplot2

Make a static density plot with ggplot2

## Usage

``` r
ggplot_densityplot(
  plotmatrices,
  experiment,
  colorby = NULL,
  palette = NULL,
  expressiontype = "expression",
  base_size = 16,
  palette_name = "Set1",
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

  Column name in `experiment` specifying how lines should be colored

- palette:

  Palette of colors, one for each unique value derived from `colorby`.

- expressiontype:

  Expression type for use in y axis label

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
