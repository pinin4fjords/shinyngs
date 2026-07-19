# Make a numeric field with selectable associated cardinality (\>, \< ..).

A wrapper around
[`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html),
providing an inline label and an associated field specifying how the
value should be applied, i.e. 'greater than this value' etc.

## Usage

``` r
cardinalNumericField(
  id,
  cardinal_id,
  label,
  value,
  cardinality = "<=",
  step = NA,
  min = NA,
  max = NA,
  tooltip = NULL
)
```

## Arguments

- id:

  ID to use for the numeric field

- cardinal_id:

  ID to use for the cardinality field

- label:

  Label

- value:

  Default value

- cardinality:

  Default cardinality

- step:

  Passed to
  [`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)

- min:

  Passed to
  [`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)

- max:

  Passed to
  [`numericInput`](https://rdrr.io/pkg/shiny/man/numericInput.html)

- tooltip:

  Optional tooltip text explaining the field, shown via a help icon next
  to the label (see
  [`with_help_icon`](https://pinin4fjords.github.io/shinyngs/reference/with_help_icon.md))

## Value

out An HTML tag object that can be rendered as HTML using as.character()
