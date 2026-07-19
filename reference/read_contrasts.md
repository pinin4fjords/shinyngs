# Read and validate a contrasts file against sample metadata

Checks: 1. No duplicate contrast IDs. Ensure that the required columns
(variable, reference, target) are present. 2. Values in the contrast
variable column exist as column names in the sample metadata. 3. If
blocking factors are supplied, checks that they are present in the
sample metadata. 4. Design matrix is full rank. 5. Warn about continuous
covariates (e.g. numeric patient IDs treated as continuous). 6. Values
of specified columns don't contain special characters. 7. Verify that
the specified reference and target values exist in the corresponding
sample metadata column. 8. Issue a warning if the reference and target
levels are identical.

## Usage

``` r
read_contrasts(
  filename,
  samples,
  variable_column = "variable",
  reference_column = "reference",
  target_column = "target",
  blocking_column = "blocking",
  convert_to_list = FALSE,
  validate_design = TRUE
)
```

## Arguments

- filename:

  Contrasts file

- samples:

  Data frame of sample information

- variable_column:

  Column in contrasts file referencing sample sheet column

- reference_column:

  Column in contrast file referencing reference level of sample sheet
  variable

- target_column:

  Column in contrast file referencing target level of sample sheet
  variable

- blocking_column:

  Colon-separated column in contrast file referencing sample sheet
  variables to be used as blocking factors

- convert_to_list:

  Convert output to a list as used internally by shinyngs?

- validate_design:

  Validate design matrix (check for NAs, full rank, numeric columns,
  special characters)? Set to FALSE to skip these checks.

## Value

output Validated contrasts data frame

## Examples

``` r
samples <- data.frame(
  condition = rep(c("control", "treated"), each = 3),
  row.names = paste0("s", 1:6)
)
contrasts_file <- tempfile(fileext = ".csv")
write.csv(
  data.frame(
    id = "treated_vs_control",
    variable = "condition",
    reference = "control",
    target = "treated"
  ),
  contrasts_file, row.names = FALSE
)
read_contrasts(contrasts_file, samples)
#>                   id  variable reference  target blocking
#> 1 treated_vs_control condition   control treated       NA
```
