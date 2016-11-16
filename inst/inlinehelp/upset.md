
#### Introduction

This page leverages the excellent 'UpStart' module of Lex, Gehlenborg et al [1]. Gene sets are derived from the supplied contrasts based on the supplied filters.

See the [UpSet documentation](http://www.caleydo.org/tools/upset/) for more information on interpretion.

#### Controls

#### Intersection

These controls dictate how the plotting is actually done. Intersections are complex with large numbers of sets, so you can restrict the number of contrasts considered. You'll also find that many intersections are to small to be useful, so you can change the number of intersections that will be displayed.

<b>Note:</b> Interpretation of the UpSet-style version of these plots can be tricky, because the columns in the plot represent the areas of a Venn/Euler plot. This means that e.g. the visible intersection of four sets is that of <i>exactly</i> four sets. It does not include those features present in other sets as well. For example if a five-set itersection is present involving those four sets and one other, its counts will not be present in the four-set intersection. Switch betwen 'all' and 'upset' modes to understand the difference.

##### Expression matrix

You may select any available matrix to use for generating comparisons, including the selection of a specified list of rows. 

##### Contrasts

The choice of contrast will most tightly dictate the appearance of the plot. Choose the sample groups to compare, and the threshold on fold change (and p/q value where applicable) to apply.

#### Export

The plot is available for download via the provided button.

#### References

* Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). <a href=\"http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html\">http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html</a>
* Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, <a href=\"https://CRAN.R-project.org/package=UpSetR\">https://CRAN.R-project.org/package=UpSetR</a>.
