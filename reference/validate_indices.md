# Validate assay indices based on a given string.

This function checks if the provided index string represents valid
assays in the given assay data. The function can handle index strings
that are comma-separated integers or assay names.

## Usage

``` r
validate_indices(
  assay_data,
  index_string,
  invert_assays = FALSE,
  prettify_names = TRUE
)
```

## Arguments

- assay_data:

  A list containing matrices as assay data.

- index_string:

  A string that can be a comma-separated list of integers or assay
  names.

- invert_assays:

  Boolean, return the indices NOT specified.

- prettify_names:

  Boolean. Prettify element names?

## Value

A vector of valid indices (either as integers or assay names).

## Examples

``` r
assay_data_example <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))
valid_assays1 <- validate_indices(assay_data_example, "1,2")
valid_assays2 <- validate_indices(assay_data_example, "a,b")
#> Error in validate_indices(assay_data_example, "a,b"): Invalid assays: A, B, valid indices are:1, 2, 3, a, b, c
```
