# Summarise an input matrix

Matrix summarization function adapted from
<http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)>the
R cookbook.

## Usage

``` r
summary_se(
  data = NULL,
  measurevar,
  groupvars = NULL,
  na.rm = FALSE,
  conf.interval = 0.95,
  add_medians = FALSE,
  .drop = TRUE
)
```

## Arguments

- data:

  A data frame. Expects all values for summarisation to be in one
  column, which may require judicious use of `melt_matrix`.

- measurevar:

  The name of a column that contains the variable to be summariezed

- groupvars:

  A vector containing names of columns that contain grouping variables

- na.rm:

  A boolean that indicates whether to ignore NA's

- conf.interval:

  The percent range of the confidence interval (default is 95 percent)

- add_medians:

  Logical indicating whether medians should be added to the output.
  Standard error estimates for the median require bootstrapping, so TRUE
  for this variables make summary statistic calculation take longer.

- .drop:

  Logical controlling whether unobserved combinations of `groupvars` are
  dropped, passed to
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Value

out Data frame with summary statistics

## Details

Uses bootstrapping to return the standard error/ deviation of the
median.

## Examples

``` r
tg <- ToothGrowth
summary_se(tg, measurevar = "len", groupvars = c("supp", "dose"))
#>   supp dose  N  mean       sd        se       ci
#> 1   OJ  0.5 10 13.23 4.459709 1.4102837 3.190283
#> 2   OJ  1.0 10 22.70 3.910953 1.2367520 2.797727
#> 3   OJ  2.0 10 26.06 2.655058 0.8396031 1.899314
#> 4   VC  0.5 10  7.98 2.746634 0.8685620 1.964824
#> 5   VC  1.0 10 16.77 2.515309 0.7954104 1.799343
#> 6   VC  2.0 10 26.14 4.797731 1.5171757 3.432090
```
