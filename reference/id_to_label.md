# Create row labels based on the settings of `labelfield` in the `ExploratorySummarizedExperiment` object and the annotation data in `mcols`.

Create row labels based on the settings of `labelfield` in the
`ExploratorySummarizedExperiment` object and the annotation data in
`mcols`.

## Usage

``` r
id_to_label(ids, ese, sep = " / ")
```

## Arguments

- ids:

  list of ids

- ese:

  An ExploratorySummarizedExperiment

- sep:

  Separator for ID and label fields

## Value

String vector of same length as `ids`

## Examples

``` r
expression <- matrix(1:12, nrow = 3,
  dimnames = list(c("ENSG1", "ENSG2", "ENSG3"), paste0("s", 1:4)))
coldata <- data.frame(
  condition = rep(c("treated", "control"), each = 2),
  row.names = paste0("s", 1:4)
)
annotation <- data.frame(
  gene_id = c("ENSG1", "ENSG2", "ENSG3"),
  gene_name = c("GeneA", "GeneB", "GeneC"),
  row.names = c("ENSG1", "ENSG2", "ENSG3")
)
ese <- ExploratorySummarizedExperiment(
  assays = list(expression = expression),
  colData = coldata,
  annotation = annotation,
  idfield = "gene_id",
  labelfield = "gene_name"
)
id_to_label(c("ENSG1", "ENSG2"), ese)
#> [1] "GeneA / ENSG1" "GeneB / ENSG2"
```
