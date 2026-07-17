# Apply shinyngs' shared plotly toolbar configuration

Gives every interactive plot the same modebar: the plotly logo is
dropped, the noisier selection buttons are removed, and the "download
plot" button produces a PNG named after the plot rather than the generic
`newplot.png`. Call once on the assembled plotly object before returning
it from a `renderPlotly()`.

## Usage

``` r
shinyngsPlotlyConfig(p, filename = "shinyngs_plot")
```

## Arguments

- p:

  A plotly object

- filename:

  Base name (no extension) for the PNG download

## Value

The plotly object with a shared `config()` applied
