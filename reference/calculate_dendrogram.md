# Calculate a clustering dendrogram based on correlation

Calculate a clustering dendrogram based on correlation

## Usage

``` r
calculate_dendrogram(
  plotmatrix,
  cor_method = "spearman",
  cluster_method = "ward.D2"
)
```

## Arguments

- plotmatrix:

  Expression/ other data matrix

- cor_method:

  'spearman' or 'perason'

- cluster_method:

  Clustering method to pass to hclust (Default: 'ward.D2')

## Value

output Object of class 'dist'

## Examples

``` r
data(airway, package = "airway")
mymatrix <- assays(airway)[[1]]
calculate_dendrogram(mymatrix)
#> 'dendrogram' with 2 branches and 8 members total, at height 0.09646692 
```
