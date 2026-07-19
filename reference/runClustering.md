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
#>             [,1]       [,2]       [,3]       [,4]
#> [1,]  0.07456498  0.3876716  0.6969634  0.3411197
#> [2,] -1.47075238 -0.0593134  0.7685329 -1.0441346
#> [3,] -0.05612874 -0.4149946 -0.7074952  1.9803999
#> Objective function:   0.8094839
#> Clustering vector:    int [1:10] 1 1 1 2 3 3 1 2 1 1
#> Cluster sizes:            6 2 2 
#> Best sample:
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> Available components:
#>  [1] "sample"     "medoids"    "i.med"      "clustering" "objective" 
#>  [6] "clusinfo"   "diss"       "call"       "silinfo"    "data"      
```
