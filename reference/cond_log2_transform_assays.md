# Conditionally apply log2 transformation on assay data based on log2_assays parameter.

Conditionally apply log2 transformation on assay data based on
log2_assays parameter.

## Usage

``` r
cond_log2_transform_assays(
  assay_data,
  log2_assays,
  threshold = 30,
  reverse = FALSE,
  prettify_names = TRUE,
  invert_assays = FALSE
)
```

## Arguments

- assay_data:

  A list containing matrices as assay data.

- log2_assays:

  A string parameter that can be NULL, empty, or a non-empty string. If
  NULL: log2 transformation will be guessed based on input assays. If
  empty: no log2 transformation will be applied. If non-empty: log2
  transformation will be applied unconditionally to specified assays.

- threshold:

  A numeric threshold to determine if the matrix should be
  log-transformed. This is only checked if should_transform is NULL.

- reverse:

  Boolean, should we unlog rather than log?

- prettify_names:

  Boolean. Prettify element names? Passed to validate_indices().

- invert_assays:

  Boolean, apply transform to assays NOT specified in log2_assays.

## Value

A modified assay_data list.

## Examples

``` r
assay_data <- list(
  counts = matrix(c(1, 100, 1000, 5, 50, 500), nrow = 3,
    dimnames = list(paste0("gene", 1:3), c("s1", "s2")))
)
cond_log2_transform_assays(assay_data, log2_assays = "1")
#> $counts
#>             s1       s2
#> gene1 0.000000 2.321928
#> gene2 6.643856 5.643856
#> gene3 9.965784 8.965784
#> 
```
