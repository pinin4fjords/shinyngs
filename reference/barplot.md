# Server function of the `barplot` module

Display grouped, stacked or overlaid bars for a matrix

## Usage

``` r
barplot(id, getPlotmatrix, getYLabel, barmode = "stack")
```

## Arguments

- id:

  Module namespace

- getPlotmatrix:

  Reactive supplying a matrix to plot

- getYLabel:

  Reactive supplying the Y axis label

- barmode:

  Bar mode: 'stack', 'group' or 'overlay'
