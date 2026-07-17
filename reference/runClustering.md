# Partition the rows of a matrix into clusters with clara()

Common function for the clustering module, validating the requested
cluster count against the number of rows available before calling
[`clara`](https://rdrr.io/pkg/cluster/man/clara.html).

## Usage

``` r
runClustering(matrix, k)
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
runClustering(matrix(rnorm(40), nrow = 10), 3)
#> Call:     cluster::clara(x = matrix, k = k, samples = 50) 
#> Medoids:
#>            [,1]        [,2]        [,3]       [,4]
#> [1,] -0.2473253 -0.05260191 -0.01595031 -0.2792372
#> [2,] -2.4372636  2.06502490 -1.30454355  0.2436855
#> [3,]  1.1484116 -1.86301149 -0.09744510 -0.1339970
#> Objective function:   1.301247
#> Clustering vector:    int [1:10] 1 1 2 3 1 3 1 1 1 1
#> Cluster sizes:            7 1 2 
#> Best sample:
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> Available components:
#>  [1] "sample"     "medoids"    "i.med"      "clustering" "objective" 
#>  [6] "clusinfo"   "diss"       "call"       "silinfo"    "data"      
```
