This page shows the output of the DEXSeq bioconductor package [1], analysing differential exon usage between specified conditions. 

#### Method overview

Please consult the DEXSeq paper and documentation (http://bioconductor.org/packages/release/bioc/vignettes/DEXSeq/inst/doc/DEXSeq.pdf) for full description of the methodology. The following is taken from the latter:

_The  basic  concept  can  be summarized as follows.  For each exon (or part of an exon) and each sample, we count how many reads map to this exon and how many reads map to any of the other exons of the same gene.  We consider the ratio of these two counts, and how it changes across conditions, to infer changes in the relative exon usage.  In  the  case  of  an  inner  exon, a  change  in  relative  exon  usage  is  typically  due  to  a  change in  the  rate  with  which  this  exon  is  spliced  into  transcripts  (alternative  splicing).  Note,  however,  that DEU is a more general concept than alternative splicing, since it also includes changes in the usage of alternative  transcript  start  sites  and  polyadenylation  sites,  which  can  cause  di erential  usage  of  exons at the 5' and 3' boundary of transcripts._

#### Table fields

The table shows both the normalised exon count and the relative exon usage per condition (as described above). Large changes in exon usage between conditions will be reflected in large fold changes and low FDR-adjusted p values.

#### Controls

The provided controls allow adjustment of the filtering options for fold change and adjusted p value. Only most significant differential exon per gene is shown by default- untick the provided checkbox to change this behaviour.

#### References

* [1] Anders S, Reyes A and Huber W (2012). &ldquo;Detecting differential usage of exons from RNA-seq data.&rdquo; <em>Genome Research</em>, <b>22</b>, pp. 4025. <a href=\"http://doi.org/10.1101/gr.133744.111\">http://doi.org/10.1101/gr.133744.111</a>.
