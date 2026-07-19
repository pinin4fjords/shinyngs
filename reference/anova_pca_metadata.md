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

## Examples

``` r
mat <- matrix(rnorm(90), nrow = 15, ncol = 6,
  dimnames = list(paste0("gene", 1:15), paste0("s", 1:6)))
pca <- compilePCAData(mat)
pcameta <- data.frame(
  condition = rep(c("treated", "control"), each = 3),
  batch = rep(c("A", "B", "C"), 2),
  row.names = colnames(mat)
)
anova_pca_metadata(pca$coords, pcameta, pca$percentVar)
#>           PC1 (29.2%) PC2 (25.4%) PC3 (21%) PC4 (13.8%) PC5 (10.6%)   PC6 (0%)
#> condition  0.01028394   0.8802899 0.5144010   0.7611769   0.8139402 0.07605518
#> batch      0.81659176   0.4715556 0.2040988   0.7279466   0.2201930 0.56645489
```
