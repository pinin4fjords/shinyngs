# Give a terse control an accessible name and a hover/focus tooltip

Icon-only or single-word controls (the dark-mode toggle, plot/table
download buttons, help-modal triggers) carry little visible text, so
they are hard to identify by mouse, keyboard or screen reader. This sets
an `aria-label` for assistive technology and wraps the control in a
short
[`bslib::tooltip()`](https://rstudio.github.io/bslib/reference/tooltip.html)
that surfaces on both hover and keyboard focus.

## Usage

``` r
a11yControl(tag, label, tooltip = label, placement = "top")
```

## Arguments

- tag:

  A single UI element to annotate

- label:

  Accessible name, applied as the tag's `aria-label`

- tooltip:

  Tooltip text shown on hover/focus; defaults to `label`

- placement:

  Tooltip placement, passed to
  [`tooltip`](https://rstudio.github.io/bslib/reference/tooltip.html)

## Value

`tag` wrapped in a
[`bslib::tooltip()`](https://rstudio.github.io/bslib/reference/tooltip.html)
