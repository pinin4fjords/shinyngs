# "PC1", "PC2", ... labels for the leading n components

Shared by
[`interactive_screeplot`](https://pinin4fjords.github.io/shinyngs/reference/interactive_screeplot.md)
and
[`anova_pca_metadata`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
so the two produce identical, identically-ordered labels - required for
[`interactive_pca_variance_heatmap`](https://pinin4fjords.github.io/shinyngs/reference/interactive_pca_variance_heatmap.md)
to line the two plots up on a shared x-axis.

## Usage

``` r
pcLabels(n, fraction_explained = NULL)
```

## Arguments

- n:

  Number of components to label

- fraction_explained:

  Optional numeric vector of percent variance explained by each
  component. When supplied, each label is suffixed with e.g. `" (45%)"`.

## Value

output A character vector of length `n`
