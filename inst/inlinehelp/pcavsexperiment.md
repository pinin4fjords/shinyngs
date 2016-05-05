#### Introduction

Principal component analysis is a dimension reduction technique designed to extract the strongest patterns from high-dimensional data (see PCA page and it's help link). But understanding what the principal components mean with relation to the experimental structure can be difficult to determine. 

In this panel, an 'analysis of variance' test is conducted for each component against each of the factors present in the experimental data. Where a principal component is closely related to a particular variable (e.g. treatment condition) it will show as a significant p value in this matrix. Ideally, the key experimental variables should show as associated with the first components, showing the highest proportion of the variance. Sometimes technical factors (e.g. sequencing lane) are associated most strongly with these components, indicating a need to account for such factors in any downstream analysis. 

#### Controls

The provided controls allow selection of the experimental variables to test, and which input matrix to use. Often a clearer separation of samples will be acheived by using the variable rows of a matrix, so the top 1000 most variant rows are used by default. You should adjust this parameter as required.

#### Plotting libraries used 

Interactive heatmaps are createding using `d3heatmap` [1]. Annotated heatmaps are provided with `pheatmap` [2]. Annotated interactive heatmaps are not currently possible. 

#### References

* [1] Cheng J and Galili T (2016). <em>d3heatmap: Interactive Heat Maps Using 'htmlwidgets' and 'D3.js'</em>. R package version 0.6.1.1, <a href=\"http://CRAN.R-project.org/package=d3heatmap\">http://CRAN.R-project.org/package=d3heatmap</a>.
* [2] Kolde R (2015). <em>pheatmap: Pretty Heatmaps</em>. R package version 1.0.7, <a href=\"http://CRAN.R-project.org/package=pheatmap\">http://CRAN.R-project.org/package=pheatmap</a>.
