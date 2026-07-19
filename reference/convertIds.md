# Convert row names to metadata identifiers

Convert row names to metadata identifiers

## Usage

``` r
convertIds(ids, ese, to, remove_na = FALSE)
```

## Arguments

- ids:

  IDs found as row names in the `ExploratorySummarizedExperiment`

- ese:

  The `ExploratorySummarizedExperiment`

- to:

  The metadata column (via `mcols`) to use

- remove_na:

  Take out NAs? Not done by default to preserve vector length

## Value

output Vector of converted ids

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
convertIds(c("ENSG1", "ENSG2"), ese, "gene_name")
#> [1] "GeneA" "GeneB"
```
