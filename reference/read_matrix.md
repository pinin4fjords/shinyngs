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
