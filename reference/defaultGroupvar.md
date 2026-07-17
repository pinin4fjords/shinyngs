# Resolve the default grouping variable for an experiment list

Returns `default_groupvar` if set, otherwise the first of `group_vars`,
or `NULL` when no grouping variables are defined.

## Usage

``` r
defaultGroupvar(eselist)
```

## Arguments

- eselist:

  ExploratorySummarizedExperimentList object

## Value

A single grouping-variable name, or `NULL`
