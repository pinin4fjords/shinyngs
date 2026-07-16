This page shows a standard expression heatmap, plotted interactively with `heatmaply` [1]. 

Clustering dendrograms are based on a correlation-based distance matrix using the Spearman coefficient, passed to the 'ward.D2' method of `hclust()` function in R [2].

#### Controls

##### Heatmap

Controls are provided to control the appearance of the heatmap. By default, colors are scaled by row to allow comparison of trends between rows.

##### Matrix

Controls are provided to select an experiment and assay, as well as the rows and columns of that matrix. You can also choose to summarise the data, creating means by sample group.

###### Gene sets

If gene sets have been uploaded, you can also use these controls to create heat maps for pre-defined gene sets from GO, KEGG etc. Just use 'Select genes by' -> 'gene set' and choose from the available options.

#### References

* [1] Galili T, O'Callaghan A, Sidi J and Sievert C (2017). <em>heatmaply: an R package for creating interactive cluster heatmaps for online publishing</em>. Bioinformatics, <a href=\"https://doi.org/10.1093/bioinformatics/btx657\">https://doi.org/10.1093/bioinformatics/btx657</a>.
* [2] Murtagh, Fionn and Legendre, Pierre (2014). <em>Ward's hierarchical agglomerative clustering method: which algorithms implement Ward's criterion?</em> Journal of Classification 31 (forthcoming).
