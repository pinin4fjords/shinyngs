# Lay out a module's controls beside its output

Wraps the standard "controls on the left, output on the right"
arrangement used by every analysis tab in a
[`bslib::layout_sidebar()`](https://rstudio.github.io/bslib/reference/sidebar.html).
The sidebar collapses on narrow screens; the main area holds the output
(typically a
[`moduleMain()`](https://pinin4fjords.github.io/shinyngs/reference/moduleMain.md)).

## Usage

``` r
moduleLayout(controls, main, width = 320)
```

## Arguments

- controls:

  Sidebar content, usually a module's input UI

- main:

  Main content, usually a
  [`moduleMain()`](https://pinin4fjords.github.io/shinyngs/reference/moduleMain.md)

- width:

  Sidebar width in pixels

## Value

A
[`bslib::layout_sidebar()`](https://rstudio.github.io/bslib/reference/sidebar.html)
