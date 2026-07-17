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
