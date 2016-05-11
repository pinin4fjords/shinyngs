This page produces plots illustrating the distribution of values in each sample, either in the form of box plots (with the `ggplot2` package of R [1]) or lines (with Plotly [3]). Very atypical distributions in one or more samples- for example a very low mean - can suggest problems that need investigation, for example on sample quality. 

#### Controls

Plotting controls are provided to adjust the graphical parameters. An important control is to select box or line plots; box plots become unmanageable at large sample numbers, so lines are the default for more than 20 samples.

Whiskers extend out to 1.5 times the interquartile range by default, and this parameter can be adjusted. Points beyond the whiskers are considered outliers, and in the Plotly-based line plots, mousing-over outliers will reveal their identity.

The expression controls allow you to select which of the matrices used in the analysis is plotted. For example it can be informative to compare raw and normalised values (where available).

#### References

* [1] Wickham H (2009). <em>ggplot2: Elegant Graphics for Data Analysis</em>. Springer-Verlag New York. ISBN 978-0-387-98140-6, <a href=\"http://had.co.nz/ggplot2/book\">http://had.co.nz/ggplot2/book</a>.
* [2] McGill, R., Tukey, J. W. and Larsen, W. A. (1978) <em>Variations of box plots.</em> The American Statistician 32, 12-16.
* [3] Sievert C, Parmer C, Hocking T, Chamberlain S, Ram K, Corvellec M and Despouy P (2015). <em>plotly: Create Interactive Web Graphics via Plotly's JavaScript Graphing Library</em>. http://CRAN.R-project.org/package=plotly
