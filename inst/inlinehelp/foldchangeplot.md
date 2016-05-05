This page shows a simple scatter plot comparing the mean expression value between two groups of samples, for all rows of a matrix, plotted with Plotly [1].

Points are shown in grey, or blue if they match fold change and q value (where available) thresholds. 

#### Controls

##### Expression matrix

Controls are provided to select with assay matrix, and which rows and columsn therein to use to contstruct the matrix to use in the plot.

##### Contrasts

The contrasts controls allow you to choose the comparison to be made, and the fold changes and q value threshold to use. 

##### Scatter plot

This field group allows basic control on the size of the points, and whether the significant point are labelled (rarely a good idea).

##### Highlight points

It can often be useful to highlight sets of genes to illustrate trends. Use this set of filters to choose a set of genes to be highlighted by color on the plot.

#### References

* [1] Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. R package version 2.0.16, <a href=\"http://CRAN.R-project.org/package=plotly\">http://CRAN.R-project.org/package=plotly</a>.
