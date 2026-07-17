# Check whether a list-type slot on an S4 object is populated

Centralises the `length(x@some_slot) > 0` idiom used in several Shiny
modules to decide whether optional data (contrasts, gene sets, read
reports, DEXSeq results etc) is available before showing or hiding UI
for it.

## Usage

``` r
has_slot_data(x, slot_name)
```

## Arguments

- x:

  An S4 object (e.g. `ExploratorySummarizedExperiment` or
  `ExploratorySummarizedExperimentList`)

- slot_name:

  Name of the slot to check

## Value

output Boolean- is the slot populated?

## Examples

``` r
data(airway, package = "airway")
ese <- as(airway, "ExploratorySummarizedExperiment")
eselist <- ExploratorySummarizedExperimentList(ese)
#> [1] "Creating ExploratorySummarizedExperimentList object"
has_slot_data(eselist, "contrasts")
#> [1] FALSE
```
