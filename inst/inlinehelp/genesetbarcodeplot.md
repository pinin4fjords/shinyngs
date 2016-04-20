#### Introduction

This page shows a barcode plot as ouput by limma's [1] `barcodeplot()` function, building on the work of Subramanian et al [2]. Please cite both if you use these plots in a publication.   

#### Controls

Controls for this page are simple:

* A select box to choose the desired gene set (single selection only)
* A contrast to chooose the comparison of interest
* An assay data selector allowing you to choose which matrix to base the results on. Note that the FDR value displayed will likely be relevant only to one of these assay matrices (check with the analyst).

#### The plot

This plot shows where members of a gene set appear in the overall list of genes ranked by fold change between a pair of conditions. Where gene sets show strong up- or down- regulation, you expect to see 'bunching up' of its members at one extreme or the other of this ranking. This is useful to make a visual check of a gene set indicated by an FDR or a p value, and may enable you to decide how interesting a result is.

The FDR is annotated to the plot (where available).

#### The table

By way of further illustration, a table is provided showing contrast details for the gene set at hand. This should enble you to chase down the origins of a significant result from gene set analysis.

#### References

* [1] Ritchie ME, Phipson B, Wu D, Hu Y, Law CW, Shi W and Smyth GK (2015). &ldquo;limma powers differential expression analyses for RNA-sequencing and microarray studies.&rdquo; <em>Nucleic Acids Research</em>, <b>43</b>(7), pp. e47.
* [2] Subramanian A, Tamayo P, Mootha VK, Mukherjee S, Ebert BL, Gillette MA, Paulovich A, Pomeroy SL, Golub TR, Lander ES, and Mesirov JP (2005). Gene set enrichment analysis: a knowledge-based approach for interpreting genome-wide expression profiles. Proc Natl Acad Sci USA, 102, 15545-15550.
