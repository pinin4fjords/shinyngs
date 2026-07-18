# Finite x/y bounds of a differential-scatter table

Threshold lines need finite endpoints to span the plotted axis, so
infinite values (from log-transforming a zero) are excluded before
taking the range.

## Usage

``` r
finiteAxisRange(table)
```

## Arguments

- table:

  Data frame whose first two columns are the plotted x and y values

## Value

A list with `xmin`, `xmax`, `ymin` and `ymax`
