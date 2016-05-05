The page produces box plots with the `ggplot2` package of R [1]. Box plots [2] allow the comparison distributions of values, for example the distribution of expression values between samples.

Whiskers extend out to 1.5 times the interquartile range from the upper and lower quartiles, with individual points shown for outliers beyond that.

Very atypical distributions in one or more samples- for example a very low mean - can suggest problems that need investigation, for example on sample quality. 

The controls provided allow you to select which of the matrices used in the analysis is plotted. For example it can be informative to compare raw and normalised values (where available).

#### References

* [1] Wickham H (2009). <em>ggplot2: Elegant Graphics for Data Analysis</em>. Springer-Verlag New York. ISBN 978-0-387-98140-6, <a href=\"http://had.co.nz/ggplot2/book\">http://had.co.nz/ggplot2/book</a>.
* [2] McGill, R., Tukey, J. W. and Larsen, W. A. (1978) <em>Variations of box plots.</em> The American Statistician 32, 12-16.
