# Run PCA on a given matrix, expected to be variance stabilised (at least log-transformed)

Run PCA on a given matrix, expected to be variance stabilised (at least
log-transformed)

## Usage

``` r
compilePCAData(matrix, ntop = NULL)
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
pca <- compilePCAData(mat)
head(pca$coords)
#>          PC1        PC2        PC3           PC4
#> s1 -2.132779  1.2956707 -2.0185394 -4.607000e-16
#> s2  2.523725  2.2584972  0.9384735  4.849222e-15
#> s3 -2.399574 -0.9533493  2.0060331 -4.829597e-16
#> s4  2.008628 -2.6008186 -0.9259672 -4.023172e-15
```
