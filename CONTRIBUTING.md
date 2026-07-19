# Contributing

## Naming convention

New code should use `snake_case` for function and variable names. This is
already the convention for I/O and validation helpers (`read_matrix()`,
`guess_foldchange_scale()`, `validate_inputs()`).

Existing exported, user-facing functions use a mix of `camelCase` and
`snake_case` (e.g. `eselistFromYAML()` vs. `eselistfromConfig()`). Renaming
these is a breaking change for anyone scripting against the package, so they
are left as-is; don't mass-rename exported functions to fit this convention.
Internal helpers and new code are not subject to that constraint and should
use `snake_case`.

Where a Shiny module passes a reactive as an argument, prefix its parameter
name with `get` (`getTitle`, `getPalette`, `getColorby`) to distinguish it
from a plain value of the same concept (e.g. the `colorby` column-name string
parameter used by the plotting/builder functions those modules call). Don't
let the same name mean "a reactive" in one layer and "a plain value" in
another.

## Module architecture

The app is built from Shiny modules. Each module `xxx` is a trio of functions,
normally in `R/xxx.R`:

- `xxxInput(id, eselist)` builds the sidebar controls,
- `xxxOutput(id)` builds the display slot (a plot or table),
- `xxx(id, eselist)` is the server, called directly and wrapping its logic in
  `shiny::moduleServer()`.

Modules compose rather than read the `eselist` directly. Data selection
(experiment, assay, rows, columns) comes from the `selectmatrix` module, whose
server returns reactives such as `selectMatrix()`, `selectColData()` and
`getExperiment()`; colour-by grouping and the palette picker come from the
`groupby` module (`getGroupby()`, `getPalette()`). A new plot module gets all of
that for free by embedding those submodules.

A plotting module is a thin adapter: the actual drawing lives in an exported,
Shiny-free standalone function (for example the `boxplot` module calls
`ggplot_boxplot()` / `plotly_boxplot()`). Put new drawing logic in an exported
`ggplot_*` / `plotly_*` function that takes plain data and give it a
`palette_name = COLORBLIND_PALETTE_NAME` default, then have the module call it.

For a full walkthrough (including a step-by-step "add a new module" recipe) see
the developer guide article, `vignettes/articles/developer.Rmd`.

### Preserve `bindCache()` scope when extracting render logic

Several module servers wrap their plot-building reactive in `bindCache(...)`,
keyed on exactly the inputs the reactive reads, so re-rendering a tab (or
another session viewing the same matrix and settings) reuses the result instead
of recomputing. When you extract render logic out of a module server into a
standalone function (the preferred direction), make sure the `bindCache()` key
still covers every input the drawing now depends on. Miss one and you either
serve stale output or silently defeat the cache.

## Running tests, lints and the document workflow

Before pushing, run the same checks CI does:

```r
# Tests
devtools::test()

# Lints and style
lintr::lint_package()
styler::style_pkg()   # or check formatting without rewriting
```

Regenerate documentation and `NAMESPACE` from the roxygen comments whenever you
add or change an exported function, and commit the results:

```r
devtools::document()
```

Run `devtools::document()` before committing so `NAMESPACE` and `man/` stay in
sync with the source. New exported functions need a complete roxygen block
(`@param`, `@return`, `@examples`, `@export`); module-trio functions follow the
existing convention of `@keywords shiny` and are generally left unexported.
