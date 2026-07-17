# Take two delimiter-separated strings and generate a named vector

Take two delimiter-separated strings and generate a named vector

## Usage

``` r
stringsToNamedVector(
  elements_string,
  names_string = NULL,
  sep = ",",
  prettify_names = TRUE,
  simplify_files = FALSE
)
```

## Arguments

- elements_string:

  String to be converted to vector elements

- names_string:

  String to be converted to vector names by default taken from
  elements_string.

- sep:

  Separator to use

- prettify_names:

  Boolean. Prettify element names?

- simplify_files:

  If elements are file and to be used to generate names, should we
  simplify by striping path and extension?

## Value

output Named character vector
