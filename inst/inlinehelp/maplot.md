When looking for differences in replicate data between groups, it is necessary to examine both the magnitude of of the difference in mean value between groups as well as the variability within each group. A mean fold change of 2 is meaningless if points within each group regularly have fold changes much greater than that. 

An MA plot illustrates the relationship between mean expression and fold changes, and overlaid with p-values from a statistical test this can be revealing. At low expression levels, only a small difference in absolute expression is required to produce a large fold change, but because this change is within the normal variability it cannot be identified as significant. As the expression level increases, variance in expression becomes a smaller and smaller component of the mean expression level, and smaller fold changes are required for inter-group differences to be deemed singificant. 

#### Controls

##### Expression matrix

Only assays with associated data in the 'tests' slot of the input data structure are available. Where only a single assay has tests (p- and q- values), no matrix selection is provided.

##### Contrasts

The contrasts controls allow you to choose the comparison to be made, and the fold changes and q value threshold to use. 

##### Scatter plot

This field group allows basic control on the size of the points, and whether the significant point are labelled (rarely a good idea).

##### Highlight points

It can often be useful to highlight sets of genes to illustrate trends. Use this set of filters to choose a set of genes to be highlighted by color on the plot.

#### References

* Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. R package version 2.0.16, <a href=\"http://CRAN.R-project.org/package=plotly\">http://CRAN.R-project.org/package=plotly</a>.
