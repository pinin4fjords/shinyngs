# Select which MA plot threshold lines to draw

The fold change filter can apply symmetrically (both up and down) or
only in one direction, depending on the cardinality operator (one of the
choices offered by
[`cardinalNumericField`](https://pinin4fjords.github.io/shinyngs/reference/cardinalNumericField.md):
`"<="`, `">="`, `">= or <= -"`, `"<= and >= -"`) and the sign of the
limit. This picks the matching subset of rows from the `lines` data
frame built in `buildMaLines`, where rows 1-2 are the fold-down
threshold and rows 3-4 the fold-up threshold.

## Usage

``` r
selectMaLines(lines, fccard, fclim)
```

## Arguments

- lines:

  data.frame of threshold lines with four rows, in the row order
  described above

- fccard:

  Fold change cardinality operator, as returned by `getFoldChangeCard()`
  from the `contrasts` module

- fclim:

  Fold change limit, as returned by `getFoldChange()` from the
  `contrasts` module

## Value

A subset of `lines`, with unused factor levels dropped
