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
  `"linear"`. See
  [`resolve_foldchange_scale`](https://pinin4fjords.github.io/shinyngs/reference/resolve_foldchange_scale.md).

- unlog_foldchanges:

  Deprecated, use `fold_change_scale` instead. Reverse a log on fold
  changes? Set to TRUE if values are logged.

## Value

output Validated selected columns of differential stats files as a data
frame, with the resolved scale attached as the `fold_change_scale`
attribute.
