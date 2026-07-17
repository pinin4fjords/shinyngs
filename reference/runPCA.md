# Run a simple PCA analysis

Common function for PCA-using parts of the app

## Usage

``` r
runPCA(matrix, do_log = TRUE)
```

## Arguments

- matrix:

  Matrix (not logged)

- do_log:

  Boolean- apply log transform to input matrix?

## Value

pca Output of the prcomp function

## Examples

``` r
runPCA(mymatrix)
#> Error in runPCA(mymatrix): could not find function "runPCA"
```
