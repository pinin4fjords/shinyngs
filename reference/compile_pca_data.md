# Run PCA on a given matrix, expected to be variance stabilised (at least log-transformed)

Run PCA on a given matrix, expected to be variance stabilised (at least
log-transformed)

## Usage

``` r
compile_pca_data(matrix, ntop = NULL)
```

## Arguments

- matrix:

  Simple matrix with genes by row and samples by column

- ntop:

  Number of most variable genes to use

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
#> s1 -2.4326269 -2.241459  1.0998364 -2.141740e-16
#> s2  0.3933174  2.651785  1.6045100 -3.023279e-15
#> s3  3.5144122 -1.423164 -0.3897731  2.280088e-15
#> s4 -1.4751027  1.012838 -2.3145732  1.015335e-15
```
