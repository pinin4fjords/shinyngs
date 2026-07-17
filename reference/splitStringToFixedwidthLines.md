# Given a string with spaces, try to split into multiple lines of \< `linewidth` characters

Given a string with spaces, try to split into multiple lines of \<
`linewidth` characters

## Usage

``` r
splitStringToFixedwidthLines(string, linewidth = 20)
```

## Arguments

- string:

  A string with spaces

- linewidth:

  The maximum line length in characters (default: 30)

## Value

A string with newline characters added where appropriate

## Examples

``` r
splitStringToFixedwidthLines("once upon a time there was a giant and a beanstalk and a pot of gold and some beans")
#> [1] "once upon a time there\nwas a giant and a beanstalk\nand a pot of gold and\nsome beans"
```
