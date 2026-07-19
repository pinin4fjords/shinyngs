# Contributing

## Naming convention

All function and variable names, exported or internal, use `snake_case`. This
was not always true: exported functions used to mix `camelCase` and
`snake_case` (e.g. `eselistFromYAML()` vs. `eselistfromConfig()`), and several
plotting functions were named after the library that renders them
(`plotly_scatterplot()`, `ggplot_boxplot()`) rather than what they do. The
`plotly_*` prefix in particular collided in spirit with `plotly`'s own
exports (`plotly_build()`, `plotly_json()`, etc.), so autocomplete and a
skim of exported symbols couldn't tell shinyngs' wrappers from plotly's own
API. That whole surface was renamed in one pass (the major version bump this
shipped in) rather than left to calcify further — don't re-introduce
camelCase or implementation-named functions in new code.

Plotting functions are named for what they produce, not for the library used
to render them: `interactive_*` for the plotly-backed versions, `static_*`
for the ggplot2/scatterplot3d-backed ones (e.g. `interactive_boxplot()` /
`static_boxplot()`).

A few exceptions to `snake_case` are intentional, not oversights: S4 class
constructors (`ExploratorySummarizedExperiment`,
`ExploratorySummarizedExperimentList`) stay `PascalCase`, matching the
standing Bioconductor/S4 convention shared with `SummarizedExperiment` and
friends.

Where a Shiny module passes a reactive as an argument, prefix its parameter
name with `get` (`getTitle`, `getPalette`, `getColorby`) to distinguish it
from a plain value of the same concept (e.g. the `colorby` column-name string
parameter used by the plotting/builder functions those modules call). Don't
let the same name mean "a reactive" in one layer and "a plain value" in
another. `getExtension()`/`getSeparator()` used to violate this themselves
(plain functions returning a plain value, not a reactive) — they're now
`file_extension()`/`guess_separator()`.

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
`static_boxplot()` / `interactive_boxplot()`). Put new drawing logic in an exported
`interactive_*` / `static_*` function that takes plain data and give it a
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
