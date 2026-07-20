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

## Examples

``` r
sample_metadata_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(sample = paste0("s", 1:4), condition = rep(c("treated", "control"), each = 2)),
  sample_metadata_file, row.names = FALSE
)
mat <- matrix(1:12, nrow = 3, dimnames = list(paste0("gene", 1:3), paste0("s", 1:4)))
matrix_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
  matrix_file, row.names = FALSE
)
validate_inputs(
  samples_metadata = sample_metadata_file,
  assay_files = matrix_file,
  sample_id_col = "sample"
)
#> Reading sample sheet at /tmp/RtmpQHu6HN/file2998357376b8.csv with ID col sample
#> Reading assay matrix /tmp/RtmpQHu6HN/file29983445546b.csv and validating against samples and features (if supplied)
#> ...  /tmp/RtmpQHu6HN/file29983445546b.csv matrix good
#> $`/tmp/RtmpQHu6HN/file2998357376b8.csv`
#>    sample condition
#> s1     s1   treated
#> s2     s2   treated
#> s3     s3   control
#> s4     s4   control
#> 
#> $assays
#> $assays$`/tmp/RtmpQHu6HN/file29983445546b.csv`
#>       s1 s2 s3 s4
#> gene1  1  4  7 10
#> gene2  2  5  8 11
#> gene3  3  6  9 12
#> 
#> 
```
