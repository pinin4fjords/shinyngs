
#### Introduction

This page leverages the excellent 'UpStart' set intersection of Lex, Gehlenborg et al [1], re-implemented for Shiny. Gene sets are derived from the supplied contrasts based on the supplied filters.

See the [UpSet documentation](http://www.caleydo.org/tools/upset/) for more information on interpretation.

#### Controls

#### Intersection

These controls dictate how the plotting is actually done. Intersections are complex with large numbers of sets, so you can restrict the number of contrasts considered. You'll also find that many intersections are too small to be useful, so you can change the number of intersections that will be displayed.

##### Intersection type

The standard behaviour of UpSet as published (and as presented by default here) is for the intersections to represent the areas of a Venn/Euler diagram. To see the consequence of this, imagine an intersection of three sets, A, B and C. The A/B area in a Venn diagram excludes the A/B/C area, so the A/B bar in an UpSet plot also excludes the A/B/C intersection. This ensures all the bars add up to the total number of entities (genes/transcripts in this case) and means that bars in the plot are exclusive, but the display can be counter-intuitive by e.g. excluding the entities from the A/B/C intersection from the A/B and B/C intersection bars.

To make the bars refelect the complete intersection of two sets, set intersection type to 'Complete', but realise that this will mean that the bars are no longer exclusive, and the values from all bars will add up to more than the total number of entities in the data.

#### Expression matrix

You may select any available matrix to use for generating comparisons, including the selection of a specified list of rows. 

##### Contrasts

The choice of contrast will most tightly dictate the appearance of the plot. Choose the sample groups to compare, and the threshold on fold change (and p/q value where applicable) to apply.

#### Export

The plot is available for download via the provided button.

#### References

* Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). <a href=\"http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html\">http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html</a>
* Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, <a href=\"https://CRAN.R-project.org/package=UpSetR\">https://CRAN.R-project.org/package=UpSetR</a>.
