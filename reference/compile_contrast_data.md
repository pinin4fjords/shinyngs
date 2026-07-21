# Compile contrast stats for inclusion in shinyngs

Compile contrast stats for inclusion in shinyngs

## Usage

``` r
compile_contrast_data(
  differential_stats_files,
  feature_id_column = NULL,
  pval_column = NULL,
  qval_column = NULL,
  fc_column = NULL,
  fold_change_scale = "auto",
  unlog_foldchanges = NULL
)
```

## Arguments

- differential_stats_files:

  Tabular files with differential stats

- feature_id_column:

  Feature identifier column in stats files

- pval_column:

  P value column in stats files

- qval_column:

  Q value column in stats files

- fc_column:

  Fold change column in stats files

- fold_change_scale:

  Scale of the values in `fc_column`: one of `"auto"` (default),
  `"log2"` or `"linear"`. Resolved once across the fold changes combined
  from all `differential_stats_files`, rather than per-file, so
  contrasts from the same experiment are treated consistently. Whenever
  the resolved scale is `"log2"`, the `fold_changes` element of the
  returned list is converted to linear scale - it will not match the raw
  file values. Pass `"linear"` for passthrough of the files' values
  as-is. See
  [`resolve_foldchange_scale`](https://pinin4fjords.github.io/shinyngs/reference/resolve_foldchange_scale.md).

- unlog_foldchanges:

  Deprecated, use `fold_change_scale` instead. Should fold change values
  be unlogged?

## Value

output A named list of data frames by statistic (`fold_changes` is
always on a linear scale, see `fold_change_scale` above; also `pvals`,
`qvals`), number of columns equal to input file number
