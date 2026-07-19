#### Introduction

This page shows a faceted boxplot for the most differentially expressed genes in a chosen contrast: one panel per gene, samples grouped by the contrast's two conditions, with an optional overlay of individual sample points (a beeswarm).

#### Controls

##### Contrasts

The contrasts controls allow you to choose the comparison to be made, and the fold change, p value and q value thresholds used to decide which genes are significant.

##### Rank genes by

Choose how genes passing the significance filters above are ranked and ordered, e.g. 'q value (ascending)' for the most significant first, or 'Fold change (ascending)'/'(descending)' to look at genes changing most in one particular direction. 'Absolute fold change (descending)' ranks by the size of the change regardless of direction, so the biggest movers up and down both appear together; '(ascending)' does the same for the smallest, most stable changes.

##### Number of top genes to plot

This control caps how many ranked genes are drawn, from most to least extreme by the chosen metric.

##### Show individual points (beeswarm)

Toggles the per-sample point overlay on each gene's boxplot.

##### Color palette

Choose a colour-blind-safe palette (the default) or an alternative for the two condition groups.

#### Plots

##### Top gene boxplots

Each panel shows the expression values for one gene, split by the contrast's two conditions, with the associated q value annotated on the panel.
