# Resolve the scale of a fold-change column, cross-checking a user declaration and a column-naming convention against the observed data

Mirrors the log2 guessing done for assay matrices
([`cond_log2_transform_matrix`](https://pinin4fjords.github.io/shinyngs/reference/cond_log2_transform_matrix.md))
but for differential statistics fold changes, and errors rather than
silently proceeding when the available signals disagree - see
<https://github.com/pinin4fjords/shinyngs/issues/125>.

## Usage

``` r
resolve_foldchange_scale(values, fc_column = NULL, declared_scale = "auto")
```

## Arguments

- values:

  Numeric vector of fold change values.

- fc_column:

  Name of the column the values came from, used for the
  `log2FoldChange`-style naming heuristic. Can be `NULL` if unknown.

- declared_scale:

  One of `"auto"` (default), `"log2"` or `"linear"`.

## Value

Either `"log2"` or `"linear"`.

## Examples

``` r
resolve_foldchange_scale(c(-2.1, 0.3, 1.8), fc_column = "log2FoldChange")
#> [1] "log2"
resolve_foldchange_scale(c(-8, 45, -120), fc_column = "FoldChange")
#> [1] "linear"
```
