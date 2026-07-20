# Build an ExploratorySummarisedExperimentList from a description provided in a list

Build an ExploratorySummarisedExperimentList from a description provided
in a list

## Usage

``` r
eselist_from_config(config, log2_assays, log2_threshold = 30)
```

## Arguments

- config:

  Hierachical named list with input components. See `eselist_from_yaml`
  for detail.

- log2_assays:

  A string parameter that can be NULL, empty, or a non-empty string. If
  NULL: log2 transformation will be guessed based on input assays. If
  empty: no log2 transformation will be applied. If non-empty: log2
  transformation will be applied unconditionally to specified assays.

- log2_threshold:

  A numeric threshold to determine if the matrix should be
  log-transformed. This is only checked if should_transform is NULL.

## Value

out An ExploratorySummarizedExperimentList object suitable for passing
to
[`prepare_app`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md)

## Examples

``` r
sample_metadata_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(sample = paste0("s", 1:4), condition = rep(c("treated", "control"), each = 2)),
  sample_metadata_file, row.names = FALSE
)
feature_metadata_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(gene_id = c("ENSG1", "ENSG2", "ENSG3"), gene_name = c("GeneA", "GeneB", "GeneC")),
  feature_metadata_file, row.names = FALSE
)
mat <- matrix(1:12, nrow = 3, dimnames = list(c("ENSG1", "ENSG2", "ENSG3"), paste0("s", 1:4)))
matrix_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
  matrix_file, row.names = FALSE
)
differential_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(
    gene_id = rownames(mat), log2FoldChange = c(1.2, -0.5, 2.1),
    pvalue = c(0.01, 0.2, 0.001), padj = c(0.02, 0.3, 0.005)
  ),
  differential_file, row.names = FALSE
)

config <- list(
  title = "Example study",
  author = "An Author",
  experiments = list(
    rnaseq = list(
      coldata = list(file = sample_metadata_file, id = "sample"),
      annotation = list(file = feature_metadata_file, id = "gene_id", label = "gene_name"),
      expression_matrices = list(expression = list(file = matrix_file, measure = "Counts"))
    )
  ),
  contrasts = list(
    comparisons = list(c("condition", "treated", "control")),
    stats = list(
      rnaseq = list(
        expression = list(
          type = "uncompiled",
          files = list(differential_file),
          feature_id_column = "gene_id",
          fc_column = "log2FoldChange",
          pval_column = "pvalue",
          qval_column = "padj",
          fold_change_scale = "log2"
        )
      )
    )
  )
)
eselist_from_config(config, log2_assays = "")
#> Constructing ExploratorySummarizedExperiments
#> Reading /tmp/RtmpQhhYuz/file29e0751f2805.csv
#> Creating ExploratorySummarizedExperimentList
#> Creating ExploratorySummarizedExperimentList object
#> An object of class "ExploratorySummarizedExperimentList"
#> [[1]]
#> class: ExploratorySummarizedExperiment 
#> dim: 3 4 
#> metadata(0):
#> assays(1): expression
#> rownames(3): ENSG1 ENSG2 ENSG3
#> rowData names(2): gene_id gene_name
#> colnames(4): s1 s2 s3 s4
#> colData names(2): sample condition
#> 
#> Slot "title":
#> [1] "Example study"
#> 
#> Slot "author":
#> [1] "An Author"
#> 
#> Slot "description":
#> [1] ""
#> 
#> Slot "static_pdf":
#> character(0)
#> 
#> Slot "group_vars":
#> [1] "condition"
#> 
#> Slot "default_groupvar":
#> [1] "condition"
#> 
#> Slot "contrasts":
#> [[1]]
#> [1] "condition" "treated"   "control"  
#> 
#> 
#> Slot "url_roots":
#> list()
#> 
#> Slot "gene_sets":
#> list()
#> 
#> Slot "gene_set_id_type":
#> character(0)
#> 
#> Slot "ensembl_species":
#> character(0)
#> 
```
