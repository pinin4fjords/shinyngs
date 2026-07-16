#### Introduction

Principal component analysis is a dimension reduction technique designed to extract the strongest patterns from high-dimensional data (see PCA page and it's help link). But understanding what the principal components mean with relation to the experimental structure can be difficult to determine. 

In this panel, an 'analysis of variance' test is conducted for each component against each of the factors present in the experimental data. Where a principal component is closely related to a particular variable (e.g. treatment condition) it will show as a significant p value in this matrix. Ideally, the key experimental variables should show as associated with the first components, showing the highest proportion of the variance. Sometimes technical factors (e.g. sequencing lane) are associated most strongly with these components, indicating a need to account for such factors in any downstream analysis. 

#### Controls

The provided controls allow selection of the experimental variables to test, and which input matrix to use. Often a clearer separation of samples will be acheived by using the variable rows of a matrix, so the top 1000 most variant rows are used by default. You should adjust this parameter as required.

#### Plotting libraries used 

This heatmap is created using `heatmaply` [1].

#### References

* [1] Galili T, O'Callaghan A, Sidi J and Sievert C (2017). <em>heatmaply: an R package for creating interactive cluster heatmaps for online publishing</em>. Bioinformatics, <a href=\"https://doi.org/10.1093/bioinformatics/btx657\">https://doi.org/10.1093/bioinformatics/btx657</a>.
