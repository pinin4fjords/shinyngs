This is a read distribution plot, showing how read data for this experiment overlap with known genomic features. It can be very informative, for example in working out why on a small proportion of reads end up contributing to transcript quantification.

A read will be counted more than once where it's spliced and assigned to multiple features.

These data are often geneated with tools like RSeQC [1] - but check with the analyst.

Bars are shown stacked by default (rather than overlaid), since feature types are (almost) independent. The plot is drawn using the Ploly library [2].

#### References

* [1] Wang, Liguo, Shengqin Wang, and Wei Li. "RSeQC: quality control of RNA-seq experiments." Bioinformatics 28.16 (2012): 2184-2185.
* [2] Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. R package version 2.0.16, <a href=\"http://CRAN.R-project.org/package=plotly\">http://CRAN.R-project.org/package=plotly</a>.
