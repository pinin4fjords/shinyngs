#### About

This application is built with [shinyngs](https://github.com/pinin4fjords/shinyngs), an R package for interactive exploration of NGS/array expression data. shinyngs itself does no analysis - it visualises matrices, metadata and differential results computed elsewhere, using the packages below.

#### Credits

##### Application framework

* [Shiny](https://shiny.posit.co/) - Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2024). _shiny: Web Application Framework for R_.
* [bslib](https://rstudio.github.io/bslib/) - Bootstrap 5 theming for Shiny.
* [shinyjs](https://deanattali.com/shinyjs/), [shinycssloaders](https://github.com/daattali/shinycssloaders) - client-side interactivity and loading spinners.

##### Tables and plots

* [DT](https://rstudio.github.io/DT/), wrapping the [DataTables](https://datatables.net/) jQuery plugin - interactive tables.
* [plotly](https://plotly.com/r/), wrapping [plotly.js](https://plotly.com/javascript/) - interactive plots.
* [heatmaply](https://talgalili.github.io/heatmaply/) - interactive heatmaps.
* [ggplot2](https://ggplot2.tidyverse.org/), [ggdendro](https://cran.r-project.org/package=ggdendro), [scatterplot3d](https://cran.r-project.org/package=scatterplot3d) - static plotting.
* [igvShiny](https://github.com/gladkia/igvShiny), wrapping the [Integrative Genomics Viewer](https://igv.org/) (igv.js) - the gene model browser.

##### Bioconductor

* [SummarizedExperiment](https://bioconductor.org/packages/SummarizedExperiment/) - the underlying data structure for expression matrices and metadata.
* [limma](https://bioconductor.org/packages/limma/) - used for some of the plotting and gene set utilities.
* [DEXSeq](https://bioconductor.org/packages/DEXSeq/) - differential exon usage table and plots, where supplied.

Full package versions and licenses for a given installation can be listed with `sessionInfo()`; the complete dependency list is in shinyngs' [DESCRIPTION](https://github.com/pinin4fjords/shinyngs/blob/develop/DESCRIPTION) file.
