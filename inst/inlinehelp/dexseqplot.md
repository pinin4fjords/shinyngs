This page shows the output of a plotting function of the DEXSeq bioconductor package [1], analysing differential exon usage between specified conditions. 

#### Method overview

Please consult the DEXSeq paper and documentation (http://bioconductor.org/packages/release/bioc/vignettes/DEXSeq/inst/doc/DEXSeq.pdf) for full description of the methodology. The following is taken from the latter:

_The  basic  concept  can  be summarized as follows.  For each exon (or part of an exon) and each sample, we count how many reads map to this exon and how many reads map to any of the other exons of the same gene.  We consider the ratio of these two counts, and how it changes across conditions, to infer changes in the relative exon usage.  In  the  case  of  an  inner  exon, a  change  in  relative  exon  usage  is  typically  due  to  a  change in  the  rate  with  which  this  exon  is  spliced  into  transcripts  (alternative  splicing).  Note,  however,  that DEU is a more general concept than alternative splicing, since it also includes changes in the usage of alternative  transcript  start  sites  and  polyadenylation  sites,  which  can  cause  di erential  usage  of  exons at the 5' and 3' boundary of transcripts._

#### Plots

The different types of plot can be turned on or off using the provided controls.

##### Expression 

This plot show expression estimates for each of the two conditions, derived from the models used by DEXSeq. 

##### Exon usage

This shows the exon usage coefficients, derived as described above. 

##### Normalised counts

This plot shows the actual (not model-derived) individual count values for each sample.

##### Gene model 

This plot shows every exon in the gene, and colors them pink where significant differential exon usage is observed.

##### Transcripts

Where multiple transcripts are present for a gene, comparing where differential exon usage occurs with the known transcript structures can be revealing in identifying differences in transcript usage. 

#### Table fields

The table shows both the normalised exon count and the relative exon usage per condition (as described above). Large changes in exon usage between conditions will be reflected in large fold changes and low FDR-adjusted p values.

#### Controls

Controls allow selection of which gene to plot, as well controlling the FDR threshold used to color exons and selecting which of the above plots to show.

#### References

* [1] Anders S, Reyes A and Huber W (2012). &ldquo;Detecting differential usage of exons from RNA-seq data.&rdquo; <em>Genome Research</em>, <b>22</b>, pp. 4025. <a href=\"http://doi.org/10.1101/gr.133744.111\">http://doi.org/10.1101/gr.133744.111</a>.
