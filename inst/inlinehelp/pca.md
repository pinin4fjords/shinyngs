#### Introduction

Principal component analysis is a dimension reduction technique designed to extract the strongest patterns from high-dimensional data. For more information refer to the references below, but essentially the objective is to boil the variation present in potentially tens of thousands of dimensions (e.g. genes) down to a limited number of 'components', each with a fixed contribution from all of the original dimensions. Each component then has 'loadings' representing the magnitude of these contributions from each dimension/gene. 

#### Controls

##### Input matrix

The PCA is calculated here based on a matrix of features (e.g. genes, transcripts - the 'dimensions' of the data) vs samples. Controls for selecting rows and columns for the input matrix are provided under 'expression'. As with hierarchical clustering, patterns are often clearest when the more variable rows of this matrix are selected as input, so the 1000 most variant rows are selected by default.    

##### PCA

The PCA controls principally serve to allow you to select which components are displayed. The first two or three (for 2D or 3D plots) components are shown by default, but in cases where the very strongest patterns are associated with unknown factors or experiment variables of lesser interest, you may wish to select other components. 

Coloring the points by the variables associated with your samples (e.g. gender, sequencing lane etc) can reveal what factors have influenced the structure in the data. In many case it's expected that samples will group by a primary treament factor (disease vs normal etc).  If the samples don't group as you expect then you're not likely to see the differential expression you may be looking for between groups. This can be for a number of resons, and it may be, for example, that you need to adjust for technical variables in your data.

#### Plots

##### Components plot

The components plot shows the first components plotted against one another, and should separate samples according to the strongest patterns in the data, hopefully (though not necessarily) by your main exerimental variables. 

##### Loading plot

The loading plot shows which rows of the input matrix contributed most to each component, by default showing the union of the 10 strongest contributers to each component. For example, where a treatment has a strong impact on the expression of a set of genes, we expect that those genes will contribute strongly to the first few components. The nature of PCA is that components should be orthogonal to one another, the the strongest loadings at each component should be different to one another.

#### References

##### Useful beginner overviews of PCA

* https://georgemdallas.wordpress.com/2013/10/30/principal-component-analysis-4-dummies-eigenvectors-eigenvalues-and-dimension-reduction/
* http://setosa.io/ev/principal-component-analysis/

##### Plotting: the Plotly library #####

Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. http://CRAN.R-project.org/package=plotly
