When looking for differences in replicate data between groups, it is necessary to examine both the magnitude of of the difference in mean value between groups as well as the variability within each group. A mean fold change of 2 is meaningless if points within each group regularly have fold changes much greater than that.

A volcano plot, and plotted on this page with Plotly [3], is designed to allow the rapid identification of large differences in replicate data [1,2]. The log(2) fold-change is shown on the x axis (preserving sign), and the -log(10) of a p-value is shown on the y axis. Points with high-magnitude changes between groups are shown at the extremes of the x axis, and where variability is low enough to make this difference statistically significant, they also appear at the extremes of the y axis, such that the top-left and top-right portions of the graph show the points of most interest. 

Points above the specified thresholds of fold change and q value are shown in blue with mouse-over data available, while points below these thresholds are shown with inactive grey points. This is done for reasons of performance- labelling 10s of 1000s of points adds a lot of data and shows the browser.

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

* [1] Cui, Xiangqin, and Gary A. Churchill.  &ldquo;Statistical tests for differential expression in cDNA microarray experiments. &ldquo; <em>Genome Biol</em> 4.4 (2003): 210.
* [2] Li, Wentian. &ldquo;Volcano plots in analyzing differential expressions with mRNA microarrays.&ldquo; <em>Journal of bioinformatics and computational biology</em> 10.06 (2012): 1231003.
* [3] Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. R package version 2.0.16, <a href=\"http://CRAN.R-project.org/package=plotly\">http://CRAN.R-project.org/package=plotly</a>.
