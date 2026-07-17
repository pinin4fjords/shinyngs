# Generate a matrix of anova values for associating principal components with categorical covariates.

Generate a matrix of anova values for associating principal components
with categorical covariates.

## Usage

``` r
anova_pca_metadata(pca_coords, pcameta, fraction_explained)
```

## Arguments

- pca_coords:

  Data frame of PCA coordinates, with samples by row and components by
  column.

- pcameta:

  Data frame of sample metadata with sample identifiers by row and
  variables by column.

- fraction_explained:

  Numeric vector containing the percent contribution to variance of each
  component

## Value

output A numeric matrix of p values
