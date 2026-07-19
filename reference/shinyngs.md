# Interactive downstream analysis with ShinyNGS.

shinyngs is a package for downstream visualisation of data stored in
`SummarizedExperiment`-like objects, produced as a end-point of
bioinformatics analysis workflows.

## Details

What follows is technical documentation for the package, use
[`vignette('shinyngs')`](https://pinin4fjords.github.io/shinyngs/articles/shinyngs.md)
to get more user-friendly information on package usage.

## Principal software dependencies

This package makes use of many packages, and is indebted to their
developers. [Shiny](http://shiny.rstudio.com/) provides the overall
framework that made the current work possible, and
[Plotly](https://plot.ly/) has been key to making the included
visualisations as dynamic and useful as they are.

`shinyngs` requires `plotly` (\>= 4.3.4), which is available on CRAN and
installed automatically as a normal package dependency.

## Input data structure

`shinyngs` defines two new data structures. The first,
[`ExploratorySummarizedExperiment`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperiment.md)
simply takes SummarizedExperiment, and adds some slots to allow extra
description around experiments. These extra slots are not compulsory,
however, and a simple SummarizedExperiment can be converted directly
into an `ExploratorySummarizedExperiment`:

     ese <- as(airway, 'ExploratorySummarizedExperiment') 

Because a single `ExploratorySummarizedExperiment` is limited to one set
of features,
[`ExploratorySummarizedExperimentList`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList.md)
was devised to allow incorporation of other feature sets. For example,
we might want to visualise expression in terms of both transcripts and
genes. `ExploratorySummarizedExperimentList` is just a list of
`ExploratorySummarizedExperiment`s, with some additional slots
pertaining to a study as a whole.

To use `shinyngs`, data is assumed to be in these formats. I'm not
terribly happy with the structure of some of the slots in these objects,
and they may be open to 'improvement' in the future.

### Example data

An example based on the Zhang et al study of neurons and glia
(http://www.jneurosci.org/content/34/36/11929.long) is included in the
package, and can be picked apart to further understand the data
structure.

## Shiny modules

The functionality of `shinyngs` revolves around
[Shiny](http://shiny.rstudio.com/)'s concept of modules
([http://shiny.rstudio.com/articles/modules.html](http://shiny.rstudio.com/articles/modules.md)),
so this bears some explanation.

Modules allow essentially object-oriented coding for Shiny, and the
hiving of of code into namespaces such that it can be re-used in
different parts of an application without interfering with itself.
`shinyngs` has many modules that provide various functionalities.

### Components

Each module will have server and UI functions that provide processing
and UI functionality respectively. For example, the `dendro` module,
which provides sample clustering dendrograms, has
[`dendroInput`](https://pinin4fjords.github.io/shinyngs/reference/dendroInput.md),
which provides user interface components to determine clustering method
etc, and
[`dendro`](https://pinin4fjords.github.io/shinyngs/reference/dendro.md),
which parses those inputs and calls the appropriate functions to
generate a dendrogram.
[`dendroOutput`](https://pinin4fjords.github.io/shinyngs/reference/dendroOutput.md)
provides elements that display the outputs when they're ready. These
functions are all present in dendro.R.

### Usage

Any application seeking to create dendrograms need only call
[`dendroInput`](https://pinin4fjords.github.io/shinyngs/reference/dendroInput.md)
[`dendroOutput`](https://pinin4fjords.github.io/shinyngs/reference/dendroOutput.md)
in the parts of their application where inputs and outputs should
appear, and then call
[`dendro`](https://pinin4fjords.github.io/shinyngs/reference/dendro.md)
directly (again using the same ID) to activate them, via
[`moduleServer`](https://rdrr.io/pkg/shiny/man/moduleServer.html).


      dendroInput(ns('dendro'), eselist)
      dendroOutput(ns('dendro'))
      dendro('dendro', eselist) 

These module functions are not currently exported, not currently being
intended for use outside `shinyngs`. This may change.

## Main functions

Usage of modules defined in `shinyngs` is envisaged to be predominantly
via
[`prepare_app`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md).
For most modules, this passes its arguments to
[`simpleApp`](https://pinin4fjords.github.io/shinyngs/reference/simpleApp.md),
which very simply takes its `ExploratorySummarizedExperimentList` and a
module name, and attempts to call the UI and server functions of that
module, creating a simple sidebar layout. Taking our dendro app and
using the `airway` example data:


      data(airway, package = 'airway')
      ese <- as(airway, 'ExploratorySummarizedExperiment')
      eselist <- ExploratorySummarizedExperimentList(ese)
      app <- prepare_app('heatmap',eselist)

Other apps, for example
[`rnaseq`](https://pinin4fjords.github.io/shinyngs/reference/rnaseq.md)
require more complex layouts and don't use `simpleApp`.

`prepare_app` returns list with UI and server applications components.
Shiny's [`shinyApp`](https://rdrr.io/pkg/shiny/man/shinyApp.html) must
then be called to actually run the app:
`shiny::shinyApp(ui = app$ui, server = app$server)`.

## Deploying to a shiny server

Assuming a `ExploratorySummarizedExperimentList` has been serialised
with `link[base]{saveRDS}`, the Shiny app can be deployed to a server in
a script called 'app.R' with content like this:


      library(shinyngs)
      mydata <- readRDS('data.rds') app <- prepare_app('rnaseq', mydata)
      shiny::shinyApp(app$ui, app$server)

## Module list

There are three varieties of module in `shinyngs`. Top-level modules
such as `rnaseq` provide complex data mining tools pulling together a
number of other modules. Some of those sub-modules can also be called
stand-alone to make heatmaps or dendrograms, for example. Other modules
just provide small components that don't make sense in isolation. This
is the full set at the time of writing:

### Top-level

- chipseq:

  Currently a near clone of `rnaseq`, to be optimised for ChIP-seq in
  due course.

### Stand-alone

These modules can also be called directly with
[`prepare_app`](https://pinin4fjords.github.io/shinyngs/reference/prepare_app.md):

- assaydatatable:

  Provide access to asay matrices of input object

- boxplot:

  Make quartile/ boxplots

- dendro:

  Make sample-clustering dendrograms.

- dexseqplot:

  Differential exon usage plot (where relevant slot populated)

- dexseqtable:

  Differential exon usage table (where relevant slot populated)

- differentialtable:

  Differential table showing summarised group values and fold changes
  etc (where contrasts slot is populated).

- experimentaltable:

  Display colData() output from selected
  `ExploratorySummarizedExperiment`

- foldchangeplot:

  Plot mean values of one condition against another (where contrasts
  slot is populated)

- gene:

  Show expression plots and other available data for selected features

- genesetanalysistable:

  Display content of the `gene_set_analyses` slot for the selected
  `ExploratorySummarizedExperiment`

- genesetbarcodeplot:

  Make a barcode plot using selected data and `limma`'s
  [`barcodeplot`](https://rdrr.io/pkg/limma/man/barcodeplot.html)

- heatmap:

  Build heatmaps from selected matrix data. By default this is an
  expression matrix, but it can also be sample vs sample correlation
  heatmaps or PCA vs covariate association heatmaps.

- maplot:

  Uses the `scatterplot` module to plot mean expression vs fold change

- pca:

  Plots 2D or 3D PCA and loading plots based on selected matrix data,
  using the `scatterplot` module

- readreports:

  Makes bar plots and tables of read counts data where provided in the
  `read_reports` slot of an `ExploratorySummarizedExperimentList`

- rowmetatable:

  Displays data from the metadata slot of a
  `ExploratorySummarizedExperiment` object (accessed via `mcols`)

- volcanoplot:

  Uses the `scatterplot` module to plot fold change vs p value (where
  necessary slots are populated

### Component-only

These modules are only valid as components of applications created by
other modules:

- barplot:

  Used by readreports, for example. Requires reactive providing input
  data

- contrasts:

  Provides data on sample group comparisons, using the `contrasts` slot
  of the input object (where available)

- geneselect:

  Provides selection filters for assay matrix rows. Used mainly by the
  `selectmatrix` module

- genesetselect:

  Uses [Selectize](http://selectize.github.io/selectize.js/) to provide
  an autocomplete field to select from available gene sets (where
  provided in the `gene_sets` slot)

- groupby:

  Provides a UI element to choose from the `group_vars` in a
  SummarizedExperment. Useful for coloring in a PCA etc

- labelselectfield:

  This module provides an input which allows filtering on the basis of
  row IDs, labels, or other data in the metadata slot of an
  `ExploratorySummarizedExperiment`

- modal:

  uses Shiny's `modalDialog()` to create overlaid text for the current
  panel which displays when a link is clicked

- plotdownload:

  Provides a download button for displayed plots

- sampleselect:

  Allows a subset of columns to be selected from the selected assay
  matrix

- scatterplot:

  A generic module for producing scatter plots using Plotly's
  [`plot_ly`](https://rdrr.io/pkg/plotly/man/plot_ly.html) method. Used
  by multiple other modules, including `foldchangeplot` and
  `volcanoplot`

- scatterplotcontrols:

  A set of controls for scatter plots. Separation of controls in this
  module allows them to be used in multiple scatter plot instances. For
  example, both PCA and loading plots can be run off the same set of
  controls

- selectmatrix:

  Provides functionality central to `shinyngs`. Uses filtes on
  experiment and assay, along with `sampleselect` and `geneselect`, to
  select parts of input matrices for use by most other `shinyngs`
  modules

- simpletable:

  A useful generic module that takes a reactive which returns a matrix,
  and both displays that table and produces a download button

- summarizematrix:

  Module which takes a matrix and a factor which groups columns, and
  summarises by a supplied method, mean by default

## See also

Useful links:

- <https://pinin4fjords.github.io/shinyngs/>

- <https://github.com/pinin4fjords/shinyngs>

- Report bugs at <https://github.com/pinin4fjords/shinyngs/issues>

## Author

**Maintainer**: Jonathan Manning <jonathan.manning@seqera.io>
