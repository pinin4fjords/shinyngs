# Accent colour for loading spinners

[`shinycssloaders::withSpinner()`](https://rdrr.io/pkg/shinycssloaders/man/withSpinner.html)
bakes its colour into a literal CSS value rather than accepting a
`var(--bs-primary)` reference, so it can't pick up the Bootstrap theme
variable directly. This returns the brand accent (`SHINYNGS_ACCENT`),
the same value the theme's `primary` colour derives from.

## Usage

``` r
shinyngsSpinnerColor()
```

## Value

A hex colour string
