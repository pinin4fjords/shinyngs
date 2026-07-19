# Read an expression matrix file and match to specified samples and features

Read an expression matrix file and match to specified samples and
features

## Usage

``` r
read_matrix(
  matrix_file,
  sample_metadata,
  feature_metadata = NULL,
  sep = NULL,
  row.names = 1
)
```

## Arguments

- matrix_file:

  Matrix file

- sample_metadata:

  Data frame of sample metadata

- feature_metadata:

  Data fraome of feature metadata

- sep:

  Sepaarator in matrix file

- row.names:

  Matrix column number or name containing feature identifiers

## Value

output Numeric matrix

## Examples

``` r
mat <- matrix(1:12, nrow = 3,
  dimnames = list(paste0("gene", 1:3), paste0("s", 1:4)))
matrix_file <- tempfile(fileext = ".tsv")
write.table(
  data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
  matrix_file, sep = "\t", row.names = FALSE, quote = FALSE
)
sample_metadata <- data.frame(
  condition = rep(c("treated", "control"), each = 2),
  row.names = paste0("s", 1:4)
)
read_matrix(matrix_file, sample_metadata)
#>       s1 s2 s3 s4
#> gene1  1  4  7 10
#> gene2  2  5  8 11
#> gene3  3  6  9 12
```
