# Make a select field for picking one or more contrasts

Make a select field for picking one or more contrasts

## Usage

``` r
makeContrastControl(
  id,
  contrasts,
  contrast_numbers,
  multiple = FALSE,
  show_controls = TRUE,
  select_all = FALSE
)
```

## Arguments

- id:

  An id to apply to form elements

- contrasts:

  A list of lists specifying contrasts.

- contrast_numbers:

  A named vector of indices corresponding to `contrasts`.

- multiple:

  Allow multiple contrasts to be selected?

- show_controls:

  Show controls? Setting to false will cause them to be hidden.

- select_all:

  Select all contrasts by default?

## Value

output An HTML tag object that can be rendered as HTML using
as.character()
