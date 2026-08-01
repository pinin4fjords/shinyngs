#### Introduction

This page shows detailed information for one or more individual genes/features, selected by label or identifier. It combines an expression bar plot with gene annotation and, where available, a genomic model of the selected gene, alongside any differential expression results for that gene across your defined contrasts.

#### Controls

##### Gene

Use the gene field to search for and select one or more genes by label or identifier. Where more than one experiment/assay is available, use the expression controls to choose which matrix values are drawn from, and the 'Color by' control to choose an experimental variable used to color the bar plot.

##### Table options

Choose how group averages are calculated in the differential-effects table.

#### Plots and tables

##### Expression bar plot

Shows the expression value of the selected gene(s) in each sample, optionally coloured by an experimental variable.

##### Differential effects

The Table tab lists differential results across every defined contrast. When at least three finite effects are available, the Plot tab shows signed log2 fold changes ordered by absolute effect. Position relative to zero indicates direction. Filled points meet the displayed q-value threshold, open points do not, and crosses have no q value.

##### Gene info

The 'info' link opens a table of the annotation data held for the selected gene(s), with links out to external resources where configured.

##### Gene model

Where the experiment has an associated Ensembl species, a 'Gene model' link is shown, opening a diagram of the gene and its transcripts fetched from Ensembl.

#### References

* Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. http://CRAN.R-project.org/package=plotly
