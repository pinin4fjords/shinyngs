# The server function of the simpletable module

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## Usage

``` r
simpletable(
  id,
  downloadMatrix = NULL,
  displayMatrix,
  pageLength = 15,
  filename,
  rownames = FALSE,
  show_controls = TRUE,
  filter = "none"
)
```

## Arguments

- id:

  Module namespace

- downloadMatrix:

  Reactive expression for retrieving the plot to supply for download
  (default: NULL, in which case `displayMatrix()` will be used)

- displayMatrix:

  Reactive expression for retrieving the plot for display

- pageLength:

  Number of items per page

- filename:

  A string to use in the name of the table download

- rownames:

  Passed to
  [`renderDataTable`](https://rdrr.io/pkg/DT/man/dataTableOutput.html).
  Display the row names of the input matrix? (default: FALSE)

- show_controls:

  Show search box, controls etc on resulting data table? (default: TRUE)

- filter:

  Passed to
  [`DT::renderDataTable()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html)

## Examples

``` r
simpletable("simpletable", my_data_frame)
#> Error in simpletable("simpletable", my_data_frame): could not find function "simpletable"
```
