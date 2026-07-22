# Run PCA on a given matrix, expected to be variance stabilised (at least log-transformed)

Run PCA on a given matrix, expected to be variance stabilised (at least
log-transformed)

## Usage

``` r
compile_pca_data(matrix, ntop = NULL, scale_features = FALSE)
```

## Arguments

- matrix:

  Simple matrix with genes by row and samples by column

- ntop:

  Number of most variable genes to use

- scale_features:

  Boolean, passed through to
  [`runPCA`](https://pinin4fjords.github.io/shinyngs/reference/runPCA.md)'s
  `scale_features` argument. Defaults to `FALSE`, appropriate for the
  variance-stabilised matrices this function is normally called with.

## Value

a list with keys 'coords' and 'percentVar' providing PCA coordinates and
fractional variance contributions, respectively.

## Examples

``` r
mat <- matrix(rnorm(60), nrow = 15, ncol = 4,
  dimnames = list(paste0("gene", 1:15), paste0("s", 1:4)))
pca <- compile_pca_data(mat)
head(pca$coords)
#>           PC1       PC2        PC3           PC4
#> s1  0.4750677  1.052459 -2.7014122 -5.362395e-16
#> s2 -0.5111300  2.915795  1.6541855  8.099498e-15
#> s3 -3.3684134 -2.140916  0.2120809  1.805407e-15
#> s4  3.4044756 -1.827338  0.8351458 -9.006108e-15
```
