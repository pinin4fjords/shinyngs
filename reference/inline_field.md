# Wrap a Shiny input so its label is displayed inline

Wrap a Shiny input so its label is displayed inline

## Usage

``` r
inline_field(field_def, label, labelwidth = 6, tooltip = NULL)
```

## Arguments

- field_def:

  A field definition with NULL set for the label property

- label:

  Field label

- labelwidth:

  With (in units out of 12) for label

- tooltip:

  Optional tooltip text explaining the field, shown via a help icon next
  to the label (see
  [`with_help_icon`](https://pinin4fjords.github.io/shinyngs/reference/with_help_icon.md))

## Value

output A UI definition that can be passed to the shinyUI function.

## Examples

``` r
inline_field(numericInput("foo", label = NULL, min = 0, max = 100, value = 50), "FOO")
#> Error in numericInput("foo", label = NULL, min = 0, max = 100, value = 50): could not find function "numericInput"
```
