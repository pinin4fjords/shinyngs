# The server function of the upset module

This module illustrates the intersection of differential sets using a
reimplementation of the `upset` tool of Lex, Gehlenborg et al. The
reimplementation was done to allow use of more dynamic components, and
to allow plotting of all elements in given intersections (rather than
assigning every item to its highest-order intersection). It also seems
to have sped things up.

## Usage

``` r
upset(id, eselist, setlimit = 16)
```

## Arguments

- id:

  Module namespace

- eselist:

  ExploratorySummarizedExperimentList object containing
  ExploratorySummarizedExperiment objects

- setlimit:

  Maximum number of sets

## Details

This function is called directly, using the same id as its UI
counterpart, and wraps its logic in `moduleServer()` (see example).

## References

Lex and Gehlenborg (2014). Points of view: Sets and intersections.
\<em\>Nature Methods\</em\> 11, 779 (2014).
[http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html](http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.md)

Gehlenborg N (2016). \<em\>UpSetR: A More Scalable Alternative to Venn
and Euler Diagrams for Visualizing Intersecting Sets\</em\>. R package
version 1.3.0, <https://CRAN.R-project.org/package=UpSetR>

## Examples

``` r
upset("myid", eselist)
#> Error in upset("myid", eselist): could not find function "upset"
```
