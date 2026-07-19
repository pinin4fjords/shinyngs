# Guess whether fold change values are on a log2 or linear scale

Shinyngs stores fold changes internally as signed linear values (the
ratio of the two conditions, with an absolute magnitude of at least 1
and the sign indicating direction - see
[`fold_change`](https://pinin4fjords.github.io/shinyngs/reference/fold_change.md)),
so a value strictly between -1 and 1 is only possible on a log2 scale
(e.g. a log2 fold change of 0.5 is a ~1.4-fold change). log2 fold
changes are also typically small and symmetric around zero, while linear
fold changes can have very large magnitudes for the same effect size
(e.g. a 1000-fold change is a log2 fold change of ~10).

## Usage

``` r
guess_foldchange_scale(values, log2_magnitude_limit = 15)
```

## Arguments

- values:

  Numeric vector of fold change values (NAs and non-finite values are
  ignored).

- log2_magnitude_limit:

  Values with an absolute magnitude greater than this are considered
  implausible for a log2 fold change (default 15, corresponding to a
  ~32,000-fold change).

## Value

One of `"log2"`, `"linear"` or `"ambiguous"`.

## Examples

``` r
guess_foldchange_scale(c(-2.1, 0.3, 1.8, -0.05))
#> [1] "log2"
guess_foldchange_scale(c(-8, 2, 45, -120))
#> [1] "linear"
guess_foldchange_scale(c(-2, 3, 5, -8))
#> [1] "ambiguous"
```
