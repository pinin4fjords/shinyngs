# Shared tab-panel `value`s targeted from the landing page

Landing-page jump links and tile drawers activate tabs client-side by
these `value`s, so each must match an explicit `value=` on the
corresponding `tabPanel` in the rnaseq/chipseq/illuminaarray apps.

## Usage

``` r
homeNavTargets()
```

## Value

A named character vector of tab `value`s
