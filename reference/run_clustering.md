# Partition the rows of a matrix into clusters with clara()

Common function for the clustering module, validating the requested
cluster count against the number of rows available before calling
[`clara`](https://rdrr.io/pkg/cluster/man/clara.html).

## Usage

``` r
run_clustering(matrix, k)
```

## Arguments

- matrix:

  Matrix with the rows to be clustered (features by row)

- k:

  Number of clusters requested

## Value

output Output of
[`cluster::clara`](https://rdrr.io/pkg/cluster/man/clara.html)

## Examples

``` r
run_clustering(matrix(rnorm(40), nrow = 10), 3)
#> Call:     cluster::clara(x = matrix, k = k, samples = 50) 
#> Medoids:
#>            [,1]       [,2]       [,3]       [,4]
#> [1,] -0.3041839 -0.5732654  0.9101742 -0.2791133
#> [2,]  0.5939462  1.5868335 -0.6545846 -0.2073807
#> [3,] -1.5235668  0.7002136  0.1580288 -0.6506964
#> Objective function:   0.9916769
#> Clustering vector:    int [1:10] 1 2 2 3 2 1 1 1 1 1
#> Cluster sizes:            6 3 1 
#> Best sample:
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> Available components:
#>  [1] "sample"     "medoids"    "i.med"      "clustering" "objective" 
#>  [6] "clusinfo"   "diss"       "call"       "silinfo"    "data"      
```
