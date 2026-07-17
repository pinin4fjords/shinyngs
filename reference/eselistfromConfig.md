# Build an ExploratorySummarisedExperimentList from a description provided in a list

Build an ExploratorySummarisedExperimentList from a description provided
in a list

## Usage

``` r
eselistfromConfig(config, log2_assays, log2_threshold = 30)
```

## Arguments

- config:

  Hierachical named list with input components. See `eselistFromYAML`
  for detail.

- log2_assays:

  A string parameter that can be NULL, empty, or a non-empty string. If
  NULL: log2 transformation will be guessed based on input assays. If
  empty: no log2 transformation will be applied. If non-empty: log2
  transformation will be applied unconditionally to specified assays.

- log2_threshold:

  A numeric threshold to determine if the matrix should be
  log-transformed. This is only checked if should_transform is NULL.

## Value

out An ExploratorySummarizedExperimentList object suitable for passing
to
[`prepareApp`](https://pinin4fjords.github.io/shinyngs/reference/prepareApp.md)
