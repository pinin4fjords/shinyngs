# Produce a simple app with controls and layout for a single module, in a shiny `sideBarLayout()`.

Internal function called by prepareApp() to make simple single-function
apps.

## Usage

``` r
simpleApp(eselist, module = NULL, ui_only = FALSE, ...)
```

## Arguments

- eselist:

  List of ExploratorySummarizedExperiment objects with assay and
  experimental data

- module:

  Character string specifying the module to use

- ui_only:

  Don't add server components (for UI testing)

- ...:

  Additional arguments passed to the module output function

## Examples

``` r
simpleApp(eselist, "heatmap", "My study name")
#> Error in simpleApp(eselist, "heatmap", "My study name"): could not find function "simpleApp"
```
