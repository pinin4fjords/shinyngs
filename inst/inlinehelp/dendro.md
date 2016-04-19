
#### Introduction

This page displays sample clustering dendrograms. Clustering dendrograms serve to show the relatedness of your samples to one another, and if the samples don't group as you expect then you're not likely to see the differential expression you may be looking for between groups. This can be for a number of resons, and it may be, for example, that you need to adjust for technical variables in your data.

#### Controls

##### Expression matrix

Dendrograms are created from a matrix of rows (e.g. genes) vs columns (e.g. samples), and you'll find controls for selecting those under 'Expression'. 

In general, a better clustering pattern is genereated from the variable rows of a matrix, and including those rows that don't vary above the background just adds noise. For this reason, the default behaviour is to select the 1000 most variant rows of the matrix.

Alternatve row selections are 'all', and 'gene sets' (if provided). If you know that a particular gene set should differentiate your samples, clustering samples by those genes can be a useful sanity check of your data.

##### Clustering controls

The dendrogram is based on 1) an estimate of the distance/similarity between rows of the matrix and 2) processing that matrix to produce a hierchical clustering. The clustering controls allow you choose how each step is performed. Adjusting which of your experimental variables is used to color samples will also be useful in interpretation.

#### References

##### Dendrogram drawing

* de Vries A and Ripley BD (2015). ggdendro: Create Dendrograms and Tree Diagrams Using 'ggplot2'. http://CRAN.R-project.org/package=ggdendro.

##### Ward's clustering method

* Murtagh, Fionn and Legendre, Pierre (2014).  Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion?  _Journal of Classification_ *31* (forthcoming).
