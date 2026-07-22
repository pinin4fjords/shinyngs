# Run a simple PCA analysis

Common function for PCA-using parts of the app

## Usage

``` r
runPCA(matrix, do_log = TRUE, scale_features = FALSE)
```

## Arguments

- matrix:

  Matrix (not logged)

- do_log:

  Boolean- apply log transform to input matrix?

- scale_features:

  Boolean, passed to [`prcomp()`](https://rdrr.io/r/stats/prcomp.html)'s
  `scale.` argument to scale each feature to unit variance before
  running the PCA. Defaults to `FALSE`, matching
  [`DESeq2::plotPCA()`](https://rdrr.io/pkg/BiocGenerics/man/plotPCA.html)'s
  convention for variance-stabilised input: VST/rlog transforms already
  equalise per-feature variance, so further scaling mostly up-weights
  noisy, near-constant features rather than revealing structure. Set to
  `TRUE` for matrices without a variance-stabilising transform, where
  features can otherwise dominate the PCA purely by having larger scale.
  (Unrelated to
  [`interactive_heatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactive_heatmap.md)'s
  `scale` argument, which selects row/column/none display scaling.)

## Value

pca Output of the prcomp function

## Examples

``` r
runPCA(mymatrix)
#> Error in runPCA(mymatrix): could not find function "runPCA"
```
