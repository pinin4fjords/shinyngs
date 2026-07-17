# Evaluate a vector of values with respect to a limit and a cardinality, being '\>', '\<' , '\> or \<-' (e.g. a fold change above a limit in + or - directions), or '\< and \>-' (not a above a limit in + or -).

Evaluate a vector of values with respect to a limit and a cardinality,
being '\>', '\<' , '\> or \<-' (e.g. a fold change above a limit in +
or - directions), or '\< and \>-' (not a above a limit in + or -).

## Usage

``` r
evaluateCardinalFilter(values, cardinality, limit)
```

## Arguments

- values:

  Vector of numeric values

- cardinality:

  Cardinality: '\>', '\<' , '\> or \<-'

- limit:

  Numeric limit

## Value

out A logical vector
