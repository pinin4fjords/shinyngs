# Apply shinyngs' shared plotly toolbar configuration

Gives every interactive plot the same modebar: the plotly logo is
dropped, the noisier selection buttons are removed, and the "download
plot" button produces an image named after the plot (rather than the
generic `newplot.png`) in the user's chosen format. Call once on the
assembled plotly object before returning it from a `renderPlotly()`.

## Usage

``` r
shinyngsPlotlyConfig(p, filename = "shinyngs_plot", format = "png")
```

## Arguments

- p:

  A plotly object

- filename:

  Base name (no extension) for the downloaded image

- format:

  Image format for the download button, e.g. `"png"` or `"svg"`

## Value

The plotly object with a shared `config()` applied
