# Contributing

## Naming convention

New code should use `snake_case` for function and variable names. This
is already the convention for I/O and validation helpers
([`read_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/read_matrix.md),
[`guess_foldchange_scale()`](https://pinin4fjords.github.io/shinyngs/reference/guess_foldchange_scale.md),
[`validate_inputs()`](https://pinin4fjords.github.io/shinyngs/reference/validate_inputs.md)).

Existing exported, user-facing functions use a mix of `camelCase` and
`snake_case`
(e.g. [`eselistFromYAML()`](https://pinin4fjords.github.io/shinyngs/reference/eselistFromYAML.md)
vs. [`eselistfromConfig()`](https://pinin4fjords.github.io/shinyngs/reference/eselistfromConfig.md)).
Renaming these is a breaking change for anyone scripting against the
package, so they are left as-is; don’t mass-rename exported functions to
fit this convention. Internal helpers and new code are not subject to
that constraint and should use `snake_case`.

Where a Shiny module passes a reactive as an argument, prefix its
parameter name with `get` (`getTitle`, `getPalette`, `getColorby`) to
distinguish it from a plain value of the same concept (e.g. the
`colorby` column-name string parameter used by the plotting/builder
functions those modules call). Don’t let the same name mean “a reactive”
in one layer and “a plain value” in another.
