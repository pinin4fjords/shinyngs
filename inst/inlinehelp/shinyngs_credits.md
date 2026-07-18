#### About

This application is built with [shinyngs](https://github.com/pinin4fjords/shinyngs), an R package for interactive exploration of NGS/array expression data. shinyngs itself does no analysis - it visualises matrices, metadata and differential results computed elsewhere, using the packages below.

#### Credits

##### Application framework

* [Shiny](https://shiny.posit.co/) - Winston Chang, Joe Cheng, JJ Allaire, Carson Sievert, Barret Schloerke, Garrick Aden-Buie, Yihui Xie, Jeff Allen, Jonathan McPherson, Alan Dipert, Barbara Borges (Posit Software).
* [bslib](https://rstudio.github.io/bslib/) - Carson Sievert, Joe Cheng, Garrick Aden-Buie (Posit Software) - Bootstrap 5 theming for Shiny.
* [shinyjs](https://deanattali.com/shinyjs/) - Dean Attali.
* [shinycssloaders](https://github.com/daattali/shinycssloaders) - Andras Sali (original author), maintained by Dean Attali.

##### Tables and plots

* [DT](https://rstudio.github.io/DT/) - Yihui Xie, Joe Cheng, Xianying Tan, Garrick Aden-Buie, wrapping the [DataTables](https://datatables.net/) jQuery plugin (SpryMedia Ltd) - interactive tables.
* [plotly](https://plotly.com/r/) - Carson Sievert, Chris Parmer, Toby Hocking, Scott Chamberlain, Karthik Ram, Marianne Corvellec, Pedro Despouy, wrapping [plotly.js](https://plotly.com/javascript/) (Plotly Technologies Inc.) - interactive plots.
* [heatmaply](https://talgalili.github.io/heatmaply/) - Tal Galili, Alan O'Callaghan - interactive heatmaps.
* [ggplot2](https://ggplot2.tidyverse.org/) - Hadley Wickham, Winston Chang, Lionel Henry, Thomas Lin Pedersen and contributors (Posit Software) - static plotting.
* [ggdendro](https://cran.r-project.org/package=ggdendro) - Andrie de Vries.
* [scatterplot3d](https://cran.r-project.org/package=scatterplot3d) - Uwe Ligges, Martin Mächler, Sarah Schnackenberg.
* [igvShiny](https://github.com/gladkia/igvShiny) - Paul Shannon, Arkadiusz Gladki, Karolina Ścigocka, wrapping the [Integrative Genomics Viewer](https://igv.org/) (igv.js, Broad Institute) - the gene model browser.

##### Bioconductor

* [SummarizedExperiment](https://bioconductor.org/packages/SummarizedExperiment/) - Martin Morgan, Valerie Obenchain, Jim Hester, Hervé Pagès - the underlying data structure for expression matrices and metadata.
* [limma](https://bioconductor.org/packages/limma/) - Gordon Smyth and contributors - used for some of the plotting and gene set utilities.
* [DEXSeq](https://bioconductor.org/packages/DEXSeq/) - Simon Anders, Alejandro Reyes - differential exon usage table and plots, where supplied.

Full package versions, licenses and complete author/contributor lists for a given installation can be listed with `sessionInfo()`; the complete dependency list is in shinyngs' [DESCRIPTION](https://github.com/pinin4fjords/shinyngs/blob/develop/DESCRIPTION) file.
