# Reshape data to the way `ggplot2` likes it

Reshape data to the way `ggplot2` likes it

## Usage

``` r
ggplotify(
  plotmatrices,
  experiment,
  colorby = NULL,
  value_type = "expression",
  annotate_samples = FALSE,
  should_transform = NULL
)
```

## Arguments

- plotmatrices:

  A matrix of values, e.g. expression data

- experiment:

  A data frame with rows matching the columns of `matrix`

- colorby:

  An optional string specifying a column from `experiment` that will be
  used to set a color column in the reshaped output.

- value_type:

  Type of data to assemble. By default this is just expression values,
  but can be 'density' to calculate expression densities.

- annotate_samples:

  Add a suffix to sample labels reflecting their group?

- should_transform:

  A boolean indicating if the log2 transformation should be applied. If
  TRUE, log2 transformation is applied unconditionally. If FALSE, no
  transformation is applied. If NULL (default), a conditional
  transformation based on threshold is applied.

## Value

A reshaped data frame

## Examples

``` r
plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)
#> Error: object 'colorby' not found
```
