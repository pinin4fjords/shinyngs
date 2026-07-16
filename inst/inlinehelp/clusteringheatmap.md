#### Introduction

This panel generates a matrix of correlation values between pairs of samples, and generates a heatmap and dendrogram based on that matrix. It's an alternative perspective on clustering to the dendrogram based directly on the underlying matrix. 

#### Controls

Annotations from the experiment data can be added using the 'Annotate with variables' control, and appear as colored bars above the heatmap with their own legend.

The remaining controls relate to selection of the matrix and the rows and columns to use to produce this plot. More variable rows produce clearer clustering, so the top 1000 most variant rows are selected by default.

#### References

* Galili T, O'Callaghan A, Sidi J and Sievert C (2017). <em>heatmaply: an R package for creating interactive cluster heatmaps for online publishing</em>. Bioinformatics, <a href=\"https://doi.org/10.1093/bioinformatics/btx657\">https://doi.org/10.1093/bioinformatics/btx657</a>.
