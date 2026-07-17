# Make a hidden input field. Handy for replacing superfluous single-value selects etc

Make a hidden input field. Handy for replacing superfluous single-value
selects etc

## Usage

``` r
hiddenInput(id, values)
```

## Arguments

- id:

  An HTML id

- values:

  The value the input should return

## Value

output HTML as output by Shiny's `HTML()`

## Examples

``` r
hiddenInput("myid", "iamavalue")
#> <input type='text' id='myid' value='iamavalue' style='display: none;'>
```
