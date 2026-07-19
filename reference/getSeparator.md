# Infer a separator from the extension of an input file

Infer a separator from the extension of an input file

## Usage

``` r
getSeparator(file)
```

## Arguments

- file:

  Input file path

## Value

output Separator character like tab or ','

## Examples

``` r
getSeparator("my_metadata.tsv")
#> [1] "\t"
```
