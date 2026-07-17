# Read a GMT-format gene set file

Parses a plain-text `.gmt` file (one gene set per line: set name,
description, then tab-separated gene identifiers - the format used by
MSigDB and similar resources) into a named list of character vectors,
one per gene set. Blank lines are ignored.

## Usage

``` r
read_gmt(file)
```

## Arguments

- file:

  Path to a `.gmt` file

## Value

A named list of character vectors of gene identifiers, one per gene set,
named after the set name in the file's first column.
