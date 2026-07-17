# Wrap a Shiny input so its label is displayed inline

Wrap a Shiny input so its label is displayed inline

## Usage

``` r
inlineField(field_def, label, labelwidth = 6)
```

## Arguments

- field_def:

  A field definition with NULL set for the label property

- label:

  Field label

- labelwidth:

  With (in units out of 12) for label

## Value

output A UI definition that can be passed to the shinyUI function.

## Examples

``` r
inlineField(numericInput("foo", label = NULL, min = 0, max = 100, value = 50), "FOO")
#> Error in numericInput("foo", label = NULL, min = 0, max = 100, value = 50): could not find function "numericInput"
```
