# Read tables of differential statistics

Read tables of differential statistics

## Usage

``` r
read_differential(
  filename,
  feature_id_column = NULL,
  pval_column = NULL,
  qval_column = NULL,
  fc_column = NULL,
  fold_change_scale = "auto",
  unlog_foldchanges = NULL
)
```

## Arguments

- filename:

  File name of file with table of differential statistics

- feature_id_column:

  Column of stats file with feature identifiers

- pval_column:

  Column of stats file with p values

- qval_column:

  Column of stats file with adjust p values/ q values

- fc_column:

  Column of stats with fold changes

- fold_change_scale:

  Scale of the values in `fc_column`: one of `"auto"` (default, infer
  and validate from the column name and data distribution), `"log2"` or
  `"linear"`. Whenever the resolved scale is `"log2"` (whether resolved
  automatically or declared explicitly), `fc_column` in the returned
  data frame is converted to linear scale - it will not match the raw
  file values. Pass `"linear"` for passthrough of the file's values
  as-is. See
  [`resolve_foldchange_scale`](https://pinin4fjords.github.io/shinyngs/reference/resolve_foldchange_scale.md).

- unlog_foldchanges:

  Deprecated, use `fold_change_scale` instead. Reverse a log on fold
  changes? Set to TRUE if values are logged.

## Value

output Validated selected columns of differential stats files as a data
frame, with the resolved scale attached as the `fold_change_scale`
attribute. `fc_column` is always on a linear scale in the returned data
frame - see `fold_change_scale` above.

## Examples

``` r
stats_file <- tempfile(fileext = ".tsv")
write.table(
  data.frame(
    gene_id = paste0("gene", 1:5),
    pvalue = c(0.001, 0.2, 0.03, 0.5, 0.008),
    padj = c(0.01, 0.4, 0.1, 0.7, 0.04),
    log2FoldChange = c(2.5, -0.1, 1.2, 0.3, -3.1)
  ),
  stats_file, sep = "\t", row.names = FALSE, quote = FALSE
)
read_differential(stats_file,
  feature_id_column = "gene_id",
  pval_column = "pvalue",
  qval_column = "padj",
  fc_column = "log2FoldChange"
)
#>       gene_id pvalue padj log2FoldChange
#> gene1   gene1  0.001 0.01       5.656854
#> gene2   gene2  0.200 0.40      -1.071773
#> gene3   gene3  0.030 0.10       2.297397
#> gene4   gene4  0.500 0.70       1.231144
#> gene5   gene5  0.008 0.04      -8.574188
```
