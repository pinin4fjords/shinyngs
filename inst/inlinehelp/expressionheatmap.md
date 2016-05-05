This page shows a standard expression heatmap, plotted with either `d3heatmap` (interactive) [1] or `pheatmap` (annotated) [2]. 

Clustering dendrograms are based on a correlation-based distance matrix using the Spearman coefficient, passed to the 'ward.D2' method of `hclust()` function in R [3].

#### Controls

##### Heatmap

Controls are provided to control the appearance of the heatmap. By default, colors are scaled by row to allow comparison of trends between rows.

##### Matrix

Controls are provided to select an experiment and assay, as well as the rows and columns of that matrix. You can also choose to summarise the data, creating means by sample group.

###### Gene sets

If gene sets have been uploaded, you can also use these controls to create heat maps for pre-defined gene sets from GO, KEGG etc. Just use 'Select genes by' -> 'gene set' and choose from the available options.

#### References

* [1] Cheng J and Galili T (2016). <em>d3heatmap: Interactive Heat Maps Using 'htmlwidgets' and 'D3.js'</em>. R package version 0.6.1.1, <a href=\"http://CRAN.R-project.org/package=d3heatmap\">http://CRAN.R-project.org/package=d3heatmap</a>.
* [2] Kolde R (2015). <em>pheatmap: Pretty Heatmaps</em>. R package version 1.0.7, <a href=\"http://CRAN.R-project.org/package=pheatmap\">http://CRAN.R-project.org/package=pheatmap</a>.
* [3] Murtagh, Fionn and Legendre, Pierre (2014). <em>Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion?</em> Journal of Classification 31 (forthcoming).
