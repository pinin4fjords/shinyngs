# Plain "PC1", "PC2", ... labels for the leading n components

Shared by
[`plotly_screeplot`](https://pinin4fjords.github.io/shinyngs/reference/plotly_screeplot.md)
and
[`anova_pca_metadata`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
so the two produce identical, identically-ordered labels - required for
[`plotly_pca_variance_heatmap`](https://pinin4fjords.github.io/shinyngs/reference/plotly_pca_variance_heatmap.md)
to line the two plots up on a shared x-axis.

## Usage

``` r
pcLabels(n)
```

## Arguments

- n:

  Number of components to label

## Value

output A character vector of length `n`
