
#### Introduction

This page allows you to examine the clustering patterns based on a specified subset of an input matrix. The matrix is re-scaled by row to generate comparable profiles, and a specified number of clusters is generated using the clara() method from the 'cluster' R package, which is a fast approximation of partioning about medoids. 

#### Controls

##### Expression matrix

You may select any available matrix to use for clustering. By default, the 1000 most variant matrix rows are used, but you may well wish to adjust this setting. 

##### Clustering

The most important clustering control is the number of clusters, and you should experiment with different values. 

Three display modes are available. It is not informative to plot every profile in a cluster overlaid on one another, and to do is coputationally expensive. But you may plot a sample of 100 profiles from each cluster. Alternatively you may plot summary statistics: the mean or median alongside an indication of the variability at each x value, using error bars or a filled line.

Using the median as the central point of each cluster requires more computatation because of the need to use bootstraps to estimate a standard error of the median, so mean is the default setting. 

#### Export

Using the provided export button, you may download a matrix of values with the clusters indicated, to allow a more thorough investigation of the genes in each cluster.

#### References

* Kaufman, L. and Rousseeuw, P.J. (1990). Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.
