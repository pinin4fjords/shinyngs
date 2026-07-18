# Select which volcano plot threshold lines to draw

The fold change filter can apply symmetrically (both up and down) or
only in one direction, depending on the cardinality operator and the
sign of the limit. This picks the matching subset of rows from the
`lines` data frame built in the `plotLines` reactive of the
`volcanoplot` module, where rows 1-2 are the fold-down threshold, rows
3-4 the fold-up threshold, and rows 5-6 the q-value threshold.

## Usage

``` r
selectVolcanoLines(lines, fccard, fclim)
```

## Arguments

- lines:

  data.frame of threshold lines with six rows, in the row order
  described above

- fccard:

  Fold change cardinality operator, as returned by `getFoldChangeCard()`
  from the `contrasts` module

- fclim:

  Fold change limit, as returned by `getFoldChange()` from the
  `contrasts` module

## Value

A subset of `lines`, with unused factor levels dropped
