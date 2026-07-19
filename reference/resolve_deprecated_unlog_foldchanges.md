# Map the deprecated `unlog_foldchanges`/`--unlog_foldchanges` argument onto `fold_change_scale`, warning if it was used

Shared by `read_differential`, `compile_contrast_data`,
`validate_inputs` and the `make_app_from_files.R`,
`differential_plots.R` and `validate_fom_components.R` scripts under
`exec/`, so the shim only needs to be written once.

## Usage

``` r
resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)
```

## Arguments

- fold_change_scale:

  The caller's current `fold_change_scale` value (used unchanged when
  `unlog_foldchanges` is `NULL`).

- unlog_foldchanges:

  The deprecated argument value, or `NULL` if it was not supplied.

## Value

The `fold_change_scale` to use.

## Examples

``` r
resolve_deprecated_unlog_foldchanges("auto", NULL)
#> [1] "auto"
resolve_deprecated_unlog_foldchanges("auto", TRUE)
#> Warning: `unlog_foldchanges`/`--unlog_foldchanges` is deprecated and will be removed in a future release; use `fold_change_scale`/`--fold_change_scale` = "log2" (to unlog) or "linear" (to leave values as-is) instead.
#> [1] "log2"
```
