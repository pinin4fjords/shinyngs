# Call the various read/ validate methods for input data surrounding an experiment

Call the various read/ validate methods for input data surrounding an
experiment

## Usage

``` r
validate_inputs(
  samples_metadata,
  assay_files,
  contrasts_file = NULL,
  features_metadata = NULL,
  sample_id_col = "sample",
  assay_names = NULL,
  differential_results = NULL,
  feature_id_col = "gene_id",
  pval_column = "pval_column",
  qval_column = "qval_column",
  fc_column = "log2FoldChange",
  fold_change_scale = "auto",
  unlog_foldchanges = NULL
)
```

## Arguments

- samples_metadata:

  Sample metadata data frame

- assay_files:

  List of assay matrices

- contrasts_file:

  Contrasts definition file

- features_metadata:

  Feature metadata data frame

- sample_id_col:

  Column of sample metadata used for identifiers

- assay_names:

  Optional comma-separated list of assay names

- differential_results:

  Optional list of differential stats files

- feature_id_col:

  Column of feature metadata used for identifiers

- pval_column:

  P value column if differential stats files specified

- qval_column:

  Q value column if differential stats files specified

- fc_column:

  Fold change column if differential stats files specified

- fold_change_scale:

  Scale of the values in `fc_column`: one of `"auto"` (default, infer
  and validate from the column name and data distribution), `"log2"` or
  `"linear"`. Each differential results file is validated independently.
  See
  [`resolve_foldchange_scale`](https://pinin4fjords.github.io/shinyngs/reference/resolve_foldchange_scale.md).

- unlog_foldchanges:

  Deprecated, use `fold_change_scale` instead. Boolean- should fold
  changes in stats files be unlogged?

## Value

output A named list with feature/ observation components
