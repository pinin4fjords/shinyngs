# Map an Ensembl species name to an igv.js genome build and, where known, a hosted Ensembl GFF3 annotation for that build

igv.js ships a handful of stock genomes with their own default
annotation track (RefSeq), which only shows canonical/collapsed
transcripts. Where a hosted, tabix-indexed Ensembl GFF3 is publicly
available for that build, we additionally load it as a second track to
show the full transcript catalog.

## Usage

``` r
geneModelGenomeInfo(ensembl_species)
```

## Arguments

- ensembl_species:

  Ensembl species definition like 'hsapiens'

## Value

A list with `genome` (an igv.js genome build name) and, optionally,
`gff3_url`/`gff3_index_url`; or `NULL` if no igv.js build is known for
`ensembl_species`.
