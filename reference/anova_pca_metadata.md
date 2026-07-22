# Generate a matrix of anova values for associating principal components with categorical covariates.

Generate a matrix of anova values for associating principal components
with categorical covariates.

## Usage

``` r
anova_pca_metadata(pca_coords, pcameta, fraction_explained, n_components = 10)
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

- n_components:

  Number of leading components to test. Clamped to the number actually
  available in `pca_coords` if that's fewer.

## Value

output A numeric matrix of p values

## Examples

``` r
mat <- matrix(rnorm(90), nrow = 15, ncol = 6,
  dimnames = list(paste0("gene", 1:15), paste0("s", 1:6)))
pca <- compile_pca_data(mat)
pcameta <- data.frame(
  condition = rep(c("treated", "control"), each = 3),
  batch = rep(c("A", "B", "C"), 2),
  row.names = colnames(mat)
)
anova_pca_metadata(pca$coords, pcameta, pca$percentVar)
#>           PC1 (36.2%) PC2 (25%) PC3 (16.4%) PC4 (13.4%)  PC5 (9%)  PC6 (0%)
#> condition 0.009998015 0.9528623 0.995902831   0.4398678 0.9224774 0.4085805
#> batch     0.976610515 0.6584137 0.002567231   0.9643998 0.1356075 0.1114647
```
