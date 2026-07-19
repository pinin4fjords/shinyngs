# Read a metadata file

Read a metadata file

## Usage

``` r
read_metadata(filename, id_col = NULL, sep = NULL, stringsAsFactors = FALSE)
```

## Arguments

- filename:

  File name

- id_col:

  Identifier column in the file

- sep:

  File separator

- stringsAsFactors:

  Passed to `read.delim`

## Value

output Data frame

## Examples

``` r
metadata_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(
    sample = paste0("s", 1:4),
    condition = rep(c("treated", "control"), each = 2)
  ),
  metadata_file, row.names = FALSE
)
read_metadata(metadata_file, id_col = "sample")
#>    sample condition
#> s1     s1   treated
#> s2     s2   treated
#> s3     s3   control
#> s4     s4   control
```
