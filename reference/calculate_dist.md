# Calculate a distance matrix based on correlation

Calculate a distance matrix based on correlation

## Usage

``` r
calculate_dist(plotmatrix, cor_method = "spearman")
```

## Arguments

- plotmatrix:

  Expression/ other data matrix

- cor_method:

  'spearman' or 'perason'

## Value

output Object of class 'dist'

## Examples

``` r
data(airway, package = "airway")
mymatrix <- assays(airway)[[1]]
calculate_dist(mymatrix)
#>            SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516 SRR1039517
#> SRR1039509 0.08585719                                                       
#> SRR1039512 0.08347515 0.09189754                                            
#> SRR1039513 0.08938218 0.09155430 0.08749819                                 
#> SRR1039516 0.08522301 0.09173659 0.08295133 0.09263262                      
#> SRR1039517 0.08959757 0.09060547 0.08775459 0.09068847 0.07964069           
#> SRR1039520 0.08324264 0.09184080 0.08292541 0.08894072 0.08476553 0.08936057
#> SRR1039521 0.08788081 0.08953557 0.08874363 0.08298156 0.08991851 0.08655817
#>            SRR1039520
#> SRR1039509           
#> SRR1039512           
#> SRR1039513           
#> SRR1039516           
#> SRR1039517           
#> SRR1039520           
#> SRR1039521 0.08315050
```
