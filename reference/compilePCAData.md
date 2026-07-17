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
