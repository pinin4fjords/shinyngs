# Assemble the summary tile specifications for an experiment list

Internal helper returning a list of tile specs. Each spec carries its
display fields (key, value, label, icon, group) and a `detail` function
that builds its drawer content on demand, so tile identity and its
detail view stay defined in one place. Optional quantities are only
included when present.

## Usage

``` r
summaryTileSpecs(eselist)
```

## Arguments

- eselist:

  ExploratorySummarizedExperimentList object

## Value

A list of tile-spec lists
