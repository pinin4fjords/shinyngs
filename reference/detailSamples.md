# Detail-drawer builders, one per tile

Each returns a list with `title`, `goto_label`, `goto_value` and a
`body` tag, consumed by the module's drawer output.

## Usage

``` r
detailSamples(eselist)

detailFeatures(eselist, esen)

detailAssays(eselist)

detailGroups(eselist)

detailContrasts(eselist)

detailGenesets(eselist)
```

## Arguments

- eselist:

  ExploratorySummarizedExperimentList object

- esen:

  Experiment name (features builder only)

## Value

A drawer-content list
