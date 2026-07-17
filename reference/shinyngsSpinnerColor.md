# Accent colour for loading spinners

[`shinycssloaders::withSpinner()`](https://rdrr.io/pkg/shinycssloaders/man/withSpinner.html)
bakes its colour into a literal CSS value rather than accepting a
`var(--bs-primary)` reference, so it can't pick up the Bootstrap theme
variable directly. This returns the same hex value used as the theme's
`primary` colour in `shinyngsPageNavbar()`, kept as a single source so
the two stay in sync.

## Usage

``` r
shinyngsSpinnerColor()
```

## Value

A hex colour string
