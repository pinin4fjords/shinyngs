This page provides summaries of reads based on information provided when creating this resource, and creates bar plots using Plotly [1]. 

#### Reports

Example reports may include:

##### Read attrition plot

Over the course of an analsyis, reads are lost relative to the full set provided by the sequencing facility. For example adapter-contaminated reads may be removed during trimming, and some reads will not align to the reference. This plot shows at what stage the reads were lost. The bars here should be shown overlapping, since the counts for each analysis stage are a subset of those for the previous.

##### Read distribution plot

How reads are distributed by genomic feature type (exon, intron etc) can be very informative regarding the type of transcripts assayed. For example a high intron content implies a large number of immature transcripts.

##### Read gene type plot

The biotype of the genes mapped to (protein coding, rRNA etc) is often useful information to have, for example when examining the level of rRNA contamination.

#### References

* [1] Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. R package version 2.0.16, <a href=\"http://CRAN.R-project.org/package=plotly\">http://CRAN.R-project.org/package=plotly</a>.