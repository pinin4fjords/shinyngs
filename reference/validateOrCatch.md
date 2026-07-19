# Evaluate an expression, converting any error into a Shiny validation message

Wraps [`tryCatch()`](https://rdrr.io/r/base/conditions.html) around an
expression so that an error raised by a computation (e.g.
[`runPCA()`](https://pinin4fjords.github.io/shinyngs/reference/runPCA.md)
or
[`runClustering()`](https://pinin4fjords.github.io/shinyngs/reference/runClustering.md)
rejecting degenerate input) surfaces as a
[`validate`](https://rdrr.io/pkg/shiny/man/validate.html) message in the
UI rather than crashing the app.

## Usage

``` r
validateOrCatch(expr)
```

## Arguments

- expr:

  Expression to evaluate

## Value

output Result of `expr`, or triggers a Shiny validation error with the
original condition's message

## Examples

``` r
validateOrCatch(1 + 1)
#> [1] 2
```
