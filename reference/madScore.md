# Calculate MAD scores as per OmicSoft

Folllows description at
https://wiki.arrayserver.com/wiki/index.php?title=CorrelationQC.pdf.

## Usage

``` r
madScore(matrix, sample_sheet = NULL, groupby = NULL, outlier_threshold = -5)
```

## Arguments

- matrix:

  Matrix with samples by column

- sample_sheet:

  Sample sheet with samples by row

- groupby:

  Sample sheet column that can be used to group samples

- outlier_threshold:

  Value below which points should be flagged as outliers, conventionally
  -5

## Value

mad_score A data frame with columns for group name, MAD score and
outlier status. A threshold of \< -5 usually indicates outliers

## Details

Not currently deployed anywhere in shinyngs, but a potential way of
flagging outliers for investigation

## Examples

``` r
mat <- matrix(rnorm(60), nrow = 6, ncol = 10,
  dimnames = list(paste0("gene", 1:6), paste0("s", 1:10)))
sample_sheet <- data.frame(
  condition = rep(c("treated", "control"), each = 5),
  row.names = colnames(mat)
)
madScore(mat, sample_sheet, groupby = "condition")
#>       group        mad outlier
#> s6  control  0.0000000   FALSE
#> s7  control -0.6744908   FALSE
#> s8  control  0.8952252   FALSE
#> s9  control  1.2379753   FALSE
#> s10 control -0.6128920   FALSE
#> s1  treated  2.0665228   FALSE
#> s2  treated  5.7174802   FALSE
#> s3  treated  0.0000000   FALSE
#> s4  treated -0.4807919   FALSE
#> s5  treated -0.6744908   FALSE
```
