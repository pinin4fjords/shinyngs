# The server function of the categorycountplot module

Lets the user tally rows of a data frame by a categorical column,
optionally split by a second, rendered via
[`interactive_barchart`](https://pinin4fjords.github.io/shinyngs/reference/interactive_barchart.md)
(the same tally that
[`interactive_count_barplot`](https://pinin4fjords.github.io/shinyngs/reference/interactive_count_barplot.md)
exposes as a standalone function). Identifier-like columns (e.g. a gene
or sample ID/name, where almost every row has its own distinct value)
are excluded, since there would be little to count beyond '1' per row.

## Usage

``` r
categorycountplot(id, getAnnotation, filename = "categorycounts")
```

## Arguments

- id:

  Module namespace

- getAnnotation:

  Reactive supplying a data frame (one row per feature or sample) whose
  categorical (non-numeric) columns are offered for counting

- filename:

  Filename stem for the exported counts table/plot image

## Examples

``` r
categorycountplot("categorycount", getAnnotation = reactive(mtcars))
#> Error in categorycountplot("categorycount", getAnnotation = reactive(mtcars)): could not find function "categorycountplot"
```
