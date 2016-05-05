#### Introduction

This panel generates a matrix of correlation values between pairs of samples, and generates a heatmap and dendrogram based on that matrix. It's an alternative perspective on clustering to the dendrogram based directly on the underlying matrix. 

#### Controls

Annotations from the experiment data can added by clicking 'annotated' in the heatmap controls- though note that the annotated plot is static rather than interactive. 

The remaining controls relate to selection of the matrix and the rows and columns to use to produce this plot. More variable rows produce clearer clustering, so the top 1000 most variant rows are selected by default.

#### References

* Cheng J and Galili T (2016). <em>d3heatmap: Interactive Heat Maps Using 'htmlwidgets' and 'D3.js'</em>. R package version 0.6.1.1, <a href=\"http://CRAN.R-project.org/package=d3heatmap\">http://CRAN.R-project.org/package=d3heatmap</a>.
* Kolde R (2015). <em>pheatmap: Pretty Heatmaps</em>. R package version 1.0.7, <a href=\"http://CRAN.R-project.org/package=pheatmap\">http://CRAN.R-project.org/package=pheatmap</a>.
