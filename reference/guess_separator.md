# Infer a separator from the extension of an input file

Infer a separator from the extension of an input file

## Usage

``` r
guess_separator(file)
```

## Arguments

- file:

  Input file path

## Value

output Separator character like tab or ','

## Examples

``` r
guess_separator("my_metadata.tsv")
#> [1] "\t"
```
