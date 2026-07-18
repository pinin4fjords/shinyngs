# Package index

## Building apps

Core classes and entry points for assembling an
ExploratorySummarizedExperimentList and turning it into a Shiny app.

- [`ExploratorySummarizedExperiment()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperiment.md)
  : ExploratorySummarizedExperiments
- [`ExploratorySummarizedExperimentList()`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList.md)
  : ExploratorySummarizedExperimentLists, containers for
  ExploratorySummarizedExperiments
- [`prepareApp()`](https://pinin4fjords.github.io/shinyngs/reference/prepareApp.md)
  : Make UI and server functions for Shiny apps based on data supplied
  as modfied SummarizedExperiments
- [`eselistFromYAML()`](https://pinin4fjords.github.io/shinyngs/reference/eselistFromYAML.md)
  : Build an ExploratorySummarisedExperimentList from a YAML description
- [`eselistfromConfig()`](https://pinin4fjords.github.io/shinyngs/reference/eselistfromConfig.md)
  : Build an ExploratorySummarisedExperimentList from a description
  provided in a list

## Reading and validating input

Helpers for reading matrices, metadata, contrasts and enrichment results
from files, and validating them for consistency.

- [`read_contrasts()`](https://pinin4fjords.github.io/shinyngs/reference/read_contrasts.md)
  : Read and validate a contrasts file against sample metadata
- [`read_differential()`](https://pinin4fjords.github.io/shinyngs/reference/read_differential.md)
  : Read tables of differential statistics
- [`read_gmt()`](https://pinin4fjords.github.io/shinyngs/reference/read_gmt.md)
  : Read a GMT-format gene set file
- [`read_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/read_matrix.md)
  : Read an expression matrix file and match to specified samples and
  features
- [`read_metadata()`](https://pinin4fjords.github.io/shinyngs/reference/read_metadata.md)
  : Read a metadata file
- [`getExtension()`](https://pinin4fjords.github.io/shinyngs/reference/getExtension.md)
  : Extract the extension of a file
- [`getSeparator()`](https://pinin4fjords.github.io/shinyngs/reference/getSeparator.md)
  : Infer a separator from the extension of an input file
- [`checkListIsSubset()`](https://pinin4fjords.github.io/shinyngs/reference/checkListIsSubset.md)
  : Check one list is a subset of another and throw an error if not
- [`validate_indices()`](https://pinin4fjords.github.io/shinyngs/reference/validate_indices.md)
  : Validate assay indices based on a given string.
- [`validate_inputs()`](https://pinin4fjords.github.io/shinyngs/reference/validate_inputs.md)
  : Call the various read/ validate methods for input data surrounding
  an experiment
- [`validateOrCatch()`](https://pinin4fjords.github.io/shinyngs/reference/validateOrCatch.md)
  : Evaluate an expression, converting any error into a Shiny validation
  message
- [`is_valid_positive_integer_vector()`](https://pinin4fjords.github.io/shinyngs/reference/is_valid_positive_integer_vector.md)
  : Check if a comma-separated string can be parsed to an integer vector
- [`singleValidMatrix()`](https://pinin4fjords.github.io/shinyngs/reference/singleValidMatrix.md)
  : Is there only one matrix to plot from this object?
- [`has_slot_data()`](https://pinin4fjords.github.io/shinyngs/reference/has_slot_data.md)
  : Check whether a list-type slot on an S4 object is populated
- [`build_enrichment_path()`](https://pinin4fjords.github.io/shinyngs/reference/build_enrichment_path.md)
  : Build path to the enrichment results
- [`chooseGroupingVariables()`](https://pinin4fjords.github.io/shinyngs/reference/chooseGroupingVariables.md)
  : Choose a valid set of grouping variables from a targets/ experiment
  data frame.

## Matrix and statistics utilities

Functions for transforming, summarising and clustering expression
matrices.

- [`bootstrapMedian()`](https://pinin4fjords.github.io/shinyngs/reference/bootstrapMedian.md)
  : Bootstrap the standard error of the median

- [`colGeomMeans()`](https://pinin4fjords.github.io/shinyngs/reference/colGeomMeans.md)
  : Geometric means by matrix column

- [`colMedians()`](https://pinin4fjords.github.io/shinyngs/reference/colMedians.md)
  : Medians by matrix column

- [`cond_log2_transform_assays()`](https://pinin4fjords.github.io/shinyngs/reference/cond_log2_transform_assays.md)
  : Conditionally apply log2 transformation on assay data based on
  log2_assays parameter.

- [`cond_log2_transform_matrix()`](https://pinin4fjords.github.io/shinyngs/reference/cond_log2_transform_matrix.md)
  : Apply log2 transformation on a matrix.

- [`foldChange()`](https://pinin4fjords.github.io/shinyngs/reference/foldChange.md)
  : Calculate fold change between two vectors

- [`geom_mean()`](https://pinin4fjords.github.io/shinyngs/reference/geom_mean.md)
  : Geometric mean

- [`guess_foldchange_scale()`](https://pinin4fjords.github.io/shinyngs/reference/guess_foldchange_scale.md)
  : Guess whether fold change values are on a log2 or linear scale

- [`madScore()`](https://pinin4fjords.github.io/shinyngs/reference/madScore.md)
  : Calculate MAD scores as per OmicSoft

- [`resolve_deprecated_unlog_foldchanges()`](https://pinin4fjords.github.io/shinyngs/reference/resolve_deprecated_unlog_foldchanges.md)
  :

  Map the deprecated `unlog_foldchanges`/`--unlog_foldchanges` argument
  onto `fold_change_scale`, warning if it was used

- [`resolve_foldchange_scale()`](https://pinin4fjords.github.io/shinyngs/reference/resolve_foldchange_scale.md)
  : Resolve the scale of a fold-change column, cross-checking a user
  declaration and a column-naming convention against the observed data

- [`runClustering()`](https://pinin4fjords.github.io/shinyngs/reference/runClustering.md)
  : Partition the rows of a matrix into clusters with clara()

- [`selectVariableGenes()`](https://pinin4fjords.github.io/shinyngs/reference/selectVariableGenes.md)
  : Generate an integer ordering to select the n most variable genes out
  of a matrix

- [`simpleSplit()`](https://pinin4fjords.github.io/shinyngs/reference/simpleSplit.md)
  : Convenience interface to strsplit()

- [`summarizeMatrix()`](https://pinin4fjords.github.io/shinyngs/reference/summarizeMatrix.md)
  : Summarise the rows of a matrix, applying a function to groups of
  cells defined by a factor

- [`summarySE()`](https://pinin4fjords.github.io/shinyngs/reference/summarySE.md)
  : Summarise an input matrix

- [`calculateDist()`](https://pinin4fjords.github.io/shinyngs/reference/calculateDist.md)
  : Calculate a distance matrix based on correlation

- [`calculateDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/calculateDendrogram.md)
  : Calculate a clustering dendrogram based on correlation

- [`clusteringDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/clusteringDendrogram.md)
  : Make a clustering dendrogram with coloring by experimental variable

- [`compilePCAData()`](https://pinin4fjords.github.io/shinyngs/reference/compilePCAData.md)
  : Run PCA on a given matrix, expected to be variance stabilised (at
  least log-transformed)

- [`anova_pca_metadata()`](https://pinin4fjords.github.io/shinyngs/reference/anova_pca_metadata.md)
  : Generate a matrix of anova values for associating principal
  components with categorical covariates.

- [`interleaveColumns()`](https://pinin4fjords.github.io/shinyngs/reference/interleaveColumns.md)
  : Interleave the columns of two matrices of equal dimensions

## Plotting functions

Standalone plotting functions reused by the Shiny modules for static and
interactive output.

- [`ggplot_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_boxplot.md)
  : Make a boxplot with coloring by experimental variable

- [`ggplot_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/ggplot_densityplot.md)
  : Make a static density plot with ggplot2

- [`ggplotify()`](https://pinin4fjords.github.io/shinyngs/reference/ggplotify.md)
  :

  Reshape data to the way `ggplot2` likes it

- [`interactiveHeatmap()`](https://pinin4fjords.github.io/shinyngs/reference/interactiveHeatmap.md)
  : Make an interactive heatmap with heatmaply

- [`plotly_barcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_barcodeplot.md)
  : Make an interactive gene set barcode plot with plotly

- [`plotly_boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_boxplot.md)
  : Make an interactive boxplot with coloring by experimental variable

- [`plotly_clusteringDendrogram()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_clusteringDendrogram.md)
  : Make an interactive clustering dendrogram colored by experimental
  variable

- [`plotly_densityplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_densityplot.md)
  : Make a dynamic density plot with plotly

- [`plotly_quartiles()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_quartiles.md)
  : Make a line-based alternative to boxplots

- [`plotly_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/plotly_scatterplot.md)
  :

  Make scatterplots with `plot_ly()`

- [`static_scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/static_scatterplot.md)
  :

  Make scatterplots with `ggplot()` or `scatterplot3d`

- [`makeColorScale()`](https://pinin4fjords.github.io/shinyngs/reference/makeColorScale.md)
  : Make a categorical colour scale of a specified length

## Identifiers, labels and gene sets

- [`convertIds()`](https://pinin4fjords.github.io/shinyngs/reference/convertIds.md)
  : Convert row names to metadata identifiers

- [`idToLabel()`](https://pinin4fjords.github.io/shinyngs/reference/idToLabel.md)
  :

  Create row labels based on the settings of `labelfield` in the
  `ExploratorySummarizedExperiment` object and the annotation data in
  `mcols`.

- [`prettifyGeneSetName()`](https://pinin4fjords.github.io/shinyngs/reference/prettifyGeneSetName.md)
  : Prettify gene set names like those from MSigDB

- [`prettifyVariablename()`](https://pinin4fjords.github.io/shinyngs/reference/prettifyVariablename.md)
  : Make machine variable names pretty for display

- [`stringsToNamedVector()`](https://pinin4fjords.github.io/shinyngs/reference/stringsToNamedVector.md)
  : Take two delimiter-separated strings and generate a named vector

## Shiny UI helpers

- [`hiddenInput()`](https://pinin4fjords.github.io/shinyngs/reference/hiddenInput.md)
  : Make a hidden input field. Handy for replacing superfluous
  single-value selects etc
- [`inlineField()`](https://pinin4fjords.github.io/shinyngs/reference/inlineField.md)
  : Wrap a Shiny input so its label is displayed inline
- [`withHelpIcon()`](https://pinin4fjords.github.io/shinyngs/reference/withHelpIcon.md)
  : Append a help icon carrying a tooltip to a label

## General utilities

- [`na.replace()`](https://pinin4fjords.github.io/shinyngs/reference/na.replace.md)
  : Replace NAs with a string for convenience

- [`nlines()`](https://pinin4fjords.github.io/shinyngs/reference/nlines.md)
  : Count the number of lines in a string

- [`pushToList()`](https://pinin4fjords.github.io/shinyngs/reference/pushToList.md)
  : Simple list push

- [`splitStringToFixedwidthLines()`](https://pinin4fjords.github.io/shinyngs/reference/splitStringToFixedwidthLines.md)
  :

  Given a string with spaces, try to split into multiple lines of \<
  `linewidth` characters

- [`ucfirst()`](https://pinin4fjords.github.io/shinyngs/reference/ucfirst.md)
  : Capitalise the first letter of a string

## Internal (Shiny module implementation)

Shiny module UI/server function pairs and other internal helpers that
back the modules above. These aren’t part of the public API and are
listed here only so the reference index build succeeds; see the package
vignette for the supported way to build and combine modules.

- [`COLORBLIND_PALETTE`](https://pinin4fjords.github.io/shinyngs/reference/COLORBLIND_PALETTE.md)
  : shinyngs' fixed categorical colour palette

- [`ExploratorySummarizedExperiment-class`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperiment-class.md)
  : The ExploratorySummarizedExperiment class

- [`` `[`( ``*`<ExploratorySummarizedExperimentList>`*`,`*`<ANY>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList-class.md)
  [`` `[`( ``*`<ExploratorySummarizedExperimentList>`*`,`*`<numeric>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList-class.md)
  [`` `[`( ``*`<ExploratorySummarizedExperimentList>`*`,`*`<logical>`*`,`*`<missing>`*`,`*`<ANY>`*`)`](https://pinin4fjords.github.io/shinyngs/reference/ExploratorySummarizedExperimentList-class.md)
  : The ExploratorySummaizedExperimentList class

- [`a11yControl()`](https://pinin4fjords.github.io/shinyngs/reference/a11yControl.md)
  : Give a terse control an accessible name and a hover/focus tooltip

- [`addPoints()`](https://pinin4fjords.github.io/shinyngs/reference/addPoints.md)
  : Add points to a plotly object

- [`addTextLabels()`](https://pinin4fjords.github.io/shinyngs/reference/addTextLabels.md)
  : Add permanent text labels to points in a plotly graph

- [`adjustLayout()`](https://pinin4fjords.github.io/shinyngs/reference/adjustLayout.md)
  : Apply layout adjustments to plotly object

- [`annotateDifferentialTable()`](https://pinin4fjords.github.io/shinyngs/reference/annotateDifferentialTable.md)
  : Annotate a differential-scatter table with colorby and label columns

- [`assaydatatable()`](https://pinin4fjords.github.io/shinyngs/reference/assaydatatable.md)
  : The server function of the assaydatatable module

- [`assaydatatableInput()`](https://pinin4fjords.github.io/shinyngs/reference/assaydatatableInput.md)
  : The UI input function of the assaydatatable module

- [`assaydatatableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/assaydatatableOutput.md)
  : The output function of the assaydatatable module

- [`barplot()`](https://pinin4fjords.github.io/shinyngs/reference/barplot.md)
  :

  Server function of the `barplot` module

- [`barplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/barplotInput.md)
  :

  Input function of the `barplot` module

- [`barplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/barplotOutput.md)
  :

  Output function of the `barplot` module

- [`bookmarkedInputValue()`](https://pinin4fjords.github.io/shinyngs/reference/bookmarkedInputValue.md)
  : Read a bookmarked input value from a restore state

- [`box_summary()`](https://pinin4fjords.github.io/shinyngs/reference/box_summary.md)
  : Summarise a vector into the statistics a box plot needs

- [`boxplot()`](https://pinin4fjords.github.io/shinyngs/reference/boxplot.md)
  : The server function of the boxplot module

- [`boxplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/boxplotInput.md)
  : The input function of the boxplot module

- [`boxplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/boxplotOutput.md)
  : The output function of the boxplot module

- [`calculatePCAFractionExplained()`](https://pinin4fjords.github.io/shinyngs/reference/calculatePCAFractionExplained.md)
  : Extract the percent variance from a PCA analysis

- [`cardinalNumericField()`](https://pinin4fjords.github.io/shinyngs/reference/cardinalNumericField.md)
  : Make a numeric field with selectable associated cardinality (\>, \<
  ..).

- [`chipseq()`](https://pinin4fjords.github.io/shinyngs/reference/chipseq.md)
  : The server function of the chipseq module. Currently a near-clone of
  the RNA-seq module, with ChIP-seq optimisations planned.

- [`chipseqInput()`](https://pinin4fjords.github.io/shinyngs/reference/chipseqInput.md)
  : The input function of the chipseq module. Currently a near-clone of
  the RNA-seq module, with ChIP-seq optimisations planned.

- [`clustering()`](https://pinin4fjords.github.io/shinyngs/reference/clustering.md)
  : The server function of the clustering module

- [`clusteringInput()`](https://pinin4fjords.github.io/shinyngs/reference/clusteringInput.md)
  : The input function of the clustering module

- [`clusteringOutput()`](https://pinin4fjords.github.io/shinyngs/reference/clusteringOutput.md)
  : The output function of the clustering module

- [`colormaker()`](https://pinin4fjords.github.io/shinyngs/reference/colormaker.md)
  : The output function of the colorby module

- [`colormakerInput()`](https://pinin4fjords.github.io/shinyngs/reference/colormakerInput.md)
  : The input function of the colorby module

- [`combinedAnnotationColors()`](https://pinin4fjords.github.io/shinyngs/reference/combinedAnnotationColors.md)
  : Build one shared color palette across every value in an annotation
  data frame

- [`compile_contrast_data()`](https://pinin4fjords.github.io/shinyngs/reference/compile_contrast_data.md)
  : Compile contrast stats for inclusion in shinyngs

- [`configureBookmarking()`](https://pinin4fjords.github.io/shinyngs/reference/configureBookmarking.md)
  : Configure URL bookmarking for the top-level session

- [`contrasts()`](https://pinin4fjords.github.io/shinyngs/reference/contrasts.md)
  : The server function of the contrasts module

- [`contrastsInput()`](https://pinin4fjords.github.io/shinyngs/reference/contrastsInput.md)
  : The input function of the contrasts module

- [`contrastsOutput()`](https://pinin4fjords.github.io/shinyngs/reference/contrastsOutput.md)
  : The output function of the contrasts module

- [`defaultGroupvar()`](https://pinin4fjords.github.io/shinyngs/reference/defaultGroupvar.md)
  : Resolve the default grouping variable for an experiment list

- [`dendro()`](https://pinin4fjords.github.io/shinyngs/reference/dendro.md)
  : The server function of the dendrogram module

- [`dendroInput()`](https://pinin4fjords.github.io/shinyngs/reference/dendroInput.md)
  : The input function of the dendrogram module

- [`dendroOutput()`](https://pinin4fjords.github.io/shinyngs/reference/dendroOutput.md)
  : The output function of the dendro module

- [`detailSamples()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  [`detailFeatures()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  [`detailAssays()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  [`detailGroups()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  [`detailContrasts()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  [`detailGenesets()`](https://pinin4fjords.github.io/shinyngs/reference/detailSamples.md)
  : Detail-drawer builders, one per tile

- [`dexseqplot()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqplot.md)
  : The server function of the dexseqplot Shiny module

- [`dexseqplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqplotInput.md)
  : The UI input function of the dexseqplot Shiny module

- [`dexseqplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqplotOutput.md)
  : The UI output function of the dexseqplot Shiny module. Produces a
  plot and a table of values.

- [`dexseqtable()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqtable.md)
  : The server function of the dexseqtable module

- [`dexseqtableInput()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqtableInput.md)
  : The UI input function of the dexseqtable module

- [`dexseqtableInputFields()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqtableInputFields.md)
  : Make input fields for producing a table of differential exon usage.
  Separated here for re-use by the dexseqplot module

- [`dexseqtableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/dexseqtableOutput.md)
  : The output function of the dexseqtable module

- [`differentialScatterInput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialScatterInput.md)
  : Shared UI input scaffolding for the differential-scatter plot
  modules

- [`differentialScatterOutput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialScatterOutput.md)
  : Shared output scaffolding for the differential-scatter plot modules

- [`differentialScatterServer()`](https://pinin4fjords.github.io/shinyngs/reference/differentialScatterServer.md)
  : Shared server logic for the differential-scatter plot modules

- [`differentialtable()`](https://pinin4fjords.github.io/shinyngs/reference/differentialtable.md)
  : The server function of the differentialtable module

- [`differentialtableInput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialtableInput.md)
  : The UI input function of the differentialtable module

- [`differentialtableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/differentialtableOutput.md)
  : The output function of the differentialtable module

- [`drawLines()`](https://pinin4fjords.github.io/shinyngs/reference/drawLines.md)
  : Overlay lines on a plotly-generated plot

- [`evaluateCardinalFilter()`](https://pinin4fjords.github.io/shinyngs/reference/evaluateCardinalFilter.md)
  : Evaluate a vector of values with respect to a limit and a
  cardinality, being '\>', '\<' , '\> or \<-' (e.g. a fold change above
  a limit in + or - directions), or '\< and \>-' (not a above a limit
  in + or -).

- [`experimenttable()`](https://pinin4fjords.github.io/shinyngs/reference/experimenttable.md)
  : The server function of the experimenttable module

- [`experimenttableInput()`](https://pinin4fjords.github.io/shinyngs/reference/experimenttableInput.md)
  : The UI input function of the experimenttable module

- [`experimenttableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/experimenttableOutput.md)
  : The output function of the experimenttable module

- [`fieldSets()`](https://pinin4fjords.github.io/shinyngs/reference/fieldSets.md)
  : Create sets of fields for display

- [`finiteAxisRange()`](https://pinin4fjords.github.io/shinyngs/reference/finiteAxisRange.md)
  : Finite x/y bounds of a differential-scatter table

- [`fixedEffectsFormula()`](https://pinin4fjords.github.io/shinyngs/reference/fixedEffectsFormula.md)
  : Remove random effects from a model formula

- [`fixedEffectsModelMatrix()`](https://pinin4fjords.github.io/shinyngs/reference/fixedEffectsModelMatrix.md)
  : Build a model matrix from the fixed-effects part of a formula

- [`foldchangeplot()`](https://pinin4fjords.github.io/shinyngs/reference/foldchangeplot.md)
  :

  The server function of the `foldchangeplot` module

- [`foldchangeplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/foldchangeplotInput.md)
  :

  The UI input function of the `foldchangeplot` module

- [`foldchangeplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/foldchangeplotOutput.md)
  :

  The output function of the `foldchangeplot` module

- [`gene()`](https://pinin4fjords.github.io/shinyngs/reference/gene.md)
  : The server function of the gene module

- [`geneBarplot()`](https://pinin4fjords.github.io/shinyngs/reference/geneBarplot.md)
  : Main function for drawing the bar plot with plotly

- [`geneInput()`](https://pinin4fjords.github.io/shinyngs/reference/geneInput.md)
  : The input function of the gene module

- [`geneModelBiotypeColors()`](https://pinin4fjords.github.io/shinyngs/reference/geneModelBiotypeColors.md)
  :

  Default color table for Ensembl transcript biotypes, used to color the
  full transcript catalog track loaded by the `gene` module's gene model
  view

- [`geneModelGenomeInfo()`](https://pinin4fjords.github.io/shinyngs/reference/geneModelGenomeInfo.md)
  : Map an Ensembl species name to an igv.js genome build and, where
  known, a hosted Ensembl GFF3 annotation for that build

- [`geneOutput()`](https://pinin4fjords.github.io/shinyngs/reference/geneOutput.md)
  : The input function of the gene module

- [`geneselect()`](https://pinin4fjords.github.io/shinyngs/reference/geneselect.md)
  : The server function of the geneselect module

- [`geneselectInput()`](https://pinin4fjords.github.io/shinyngs/reference/geneselectInput.md)
  : The UI input function of the geneselect module

- [`genesetanalysistable()`](https://pinin4fjords.github.io/shinyngs/reference/genesetanalysistable.md)
  : The server function of the genesetanalysistable module

- [`genesetanalysistableInput()`](https://pinin4fjords.github.io/shinyngs/reference/genesetanalysistableInput.md)
  : The UI input function of the genesetanalysistable module

- [`genesetanalysistableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/genesetanalysistableOutput.md)
  : The output function of the genesetanalysistable module

- [`genesetbarcodeplot()`](https://pinin4fjords.github.io/shinyngs/reference/genesetbarcodeplot.md)
  : The server function of the genesetbarcodeplot module

- [`genesetbarcodeplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/genesetbarcodeplotInput.md)
  : The UI input function of the genesetbarcodeplot module

- [`genesetbarcodeplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/genesetbarcodeplotOutput.md)
  : The output function of the genesetbarcodeplot module

- [`genesetselect()`](https://pinin4fjords.github.io/shinyngs/reference/genesetselect.md)
  : The server function of the genesetselect module

- [`genesetselectInput()`](https://pinin4fjords.github.io/shinyngs/reference/genesetselectInput.md)
  : The UI function of the genesetselect module

- [`groupby()`](https://pinin4fjords.github.io/shinyngs/reference/groupby.md)
  : The server function of the groupby module

- [`groupbyInput()`](https://pinin4fjords.github.io/shinyngs/reference/groupbyInput.md)
  : The UI function of the groupby module

- [`heatmap()`](https://pinin4fjords.github.io/shinyngs/reference/heatmap.md)
  : The server function of the heatmap module

- [`heatmapInput()`](https://pinin4fjords.github.io/shinyngs/reference/heatmapInput.md)
  : The input function of the heatmap module

- [`heatmapOutput()`](https://pinin4fjords.github.io/shinyngs/reference/heatmapOutput.md)
  : The output function of the heatmap module

- [`homeNavTargets()`](https://pinin4fjords.github.io/shinyngs/reference/homeNavTargets.md)
  :

  Shared tab-panel `value`s targeted from the landing page

- [`homeTab()`](https://pinin4fjords.github.io/shinyngs/reference/homeTab.md)
  : Build the landing ("Home") tab for a full shinyngs application

- [`illuminaarray()`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarray.md)
  : The server function of the illuminaarray module

- [`illuminaarrayInput()`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarrayInput.md)
  : The input function of the illuminaarray module

- [`illuminaarrayqc()`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarrayqc.md)
  : The server function of the illuminaarrayqc module

- [`illuminaarrayqcInput()`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarrayqcInput.md)
  : The input function of the illuminaarrayqc module

- [`illuminaarrayqcOutput()`](https://pinin4fjords.github.io/shinyngs/reference/illuminaarrayqcOutput.md)
  : The output function of the illuminaarrayqc module

- [`labelMatrix()`](https://pinin4fjords.github.io/shinyngs/reference/labelMatrix.md)
  : Add columns to display ID and label in a table

- [`labelselectfield()`](https://pinin4fjords.github.io/shinyngs/reference/labelselectfield.md)
  :

  The server function of the `labelselectfield` module

- [`labelselectfieldInput()`](https://pinin4fjords.github.io/shinyngs/reference/labelselectfieldInput.md)
  :

  The input function of the `labelselectfield` module

- [`linkMatrix()`](https://pinin4fjords.github.io/shinyngs/reference/linkMatrix.md)
  : Add links to a table

- [`makeContrastControl()`](https://pinin4fjords.github.io/shinyngs/reference/makeContrastControl.md)
  : Make a select field for picking one or more contrasts

- [`makeContrastFilterSet()`](https://pinin4fjords.github.io/shinyngs/reference/makeContrastFilterSet.md)
  : Make a complete set of filters for a contrast: the contrast itself,
  fold change, and where applicable p- and q- values.

- [`makePackageCitation()`](https://pinin4fjords.github.io/shinyngs/reference/makePackageCitation.md)
  : Return a usable citation string for a package

- [`maplot()`](https://pinin4fjords.github.io/shinyngs/reference/maplot.md)
  :

  The server function of the `maplot` module

- [`maplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/maplotInput.md)
  :

  The UI input function of the `maplot` module

- [`maplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/maplotOutput.md)
  :

  The output function of the `maplot` module

- [`modalInput()`](https://pinin4fjords.github.io/shinyngs/reference/modalInput.md)
  :

  The input function for the `modal` module

- [`modalServer()`](https://pinin4fjords.github.io/shinyngs/reference/modalServer.md)
  :

  The server function of the `modal` module

- [`moduleLayout()`](https://pinin4fjords.github.io/shinyngs/reference/moduleLayout.md)
  : Lay out a module's controls beside its output

- [`moduleMain()`](https://pinin4fjords.github.io/shinyngs/reference/moduleMain.md)
  : Assemble a module's main-panel content

- [`navLink()`](https://pinin4fjords.github.io/shinyngs/reference/navLink.md)
  : Build a link that activates a navbar tab client-side

- [`pca()`](https://pinin4fjords.github.io/shinyngs/reference/pca.md) :
  The server function of the pca module

- [`pcaInput()`](https://pinin4fjords.github.io/shinyngs/reference/pcaInput.md)
  : The input function of the pca module

- [`pcaOutput()`](https://pinin4fjords.github.io/shinyngs/reference/pcaOutput.md)
  : The output function of the pca module

- [`plotdownload()`](https://pinin4fjords.github.io/shinyngs/reference/plotdownload.md)
  : The server function of the gene set module

- [`plotdownloadInput()`](https://pinin4fjords.github.io/shinyngs/reference/plotdownloadInput.md)
  : The input function of the gene plotdownload module

- [`readreports()`](https://pinin4fjords.github.io/shinyngs/reference/readreports.md)
  :

  Server function of the `readreports` module

- [`readreportsInput()`](https://pinin4fjords.github.io/shinyngs/reference/readreportsInput.md)
  :

  Input function of the `readreports` module

- [`readreportsOutput()`](https://pinin4fjords.github.io/shinyngs/reference/readreportsOutput.md)
  :

  Output function of the `readreports` module

- [`rnaseq()`](https://pinin4fjords.github.io/shinyngs/reference/rnaseq.md)
  : The server function of the rnaseq module

- [`rnaseqInput()`](https://pinin4fjords.github.io/shinyngs/reference/rnaseqInput.md)
  : The input function of the rnaseq module

- [`rowmetatable()`](https://pinin4fjords.github.io/shinyngs/reference/rowmetatable.md)
  : The server function of the rowmetatable module

- [`rowmetatableInput()`](https://pinin4fjords.github.io/shinyngs/reference/rowmetatableInput.md)
  : The UI input function of the rowmetatable module

- [`rowmetatableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/rowmetatableOutput.md)
  : The output function of the rowmetatable module

- [`runPCA()`](https://pinin4fjords.github.io/shinyngs/reference/runPCA.md)
  : Run a simple PCA analysis

- [`sampleselect()`](https://pinin4fjords.github.io/shinyngs/reference/sampleselect.md)
  : The server function of the sampleselect module

- [`sampleselectInput()`](https://pinin4fjords.github.io/shinyngs/reference/sampleselectInput.md)
  : The UI input function of the sampleselect module

- [`scatterplot()`](https://pinin4fjords.github.io/shinyngs/reference/scatterplot.md)
  : Server function for the scatterplot module

- [`scatterplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotInput.md)
  : Input function for the scatterplot module

- [`scatterplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotOutput.md)
  : Output function for the scatterplot module

- [`scatterplotcontrols()`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrols.md)
  : Server function for scatterplotcontrols module

- [`scatterplotcontrolsInput()`](https://pinin4fjords.github.io/shinyngs/reference/scatterplotcontrolsInput.md)
  : Input function for scatterplotcontrols module

- [`selectFoldchangeLines()`](https://pinin4fjords.github.io/shinyngs/reference/selectFoldchangeLines.md)
  : Select which fold change plot threshold lines to draw

- [`selectMaLines()`](https://pinin4fjords.github.io/shinyngs/reference/selectMaLines.md)
  : Select which MA plot threshold lines to draw

- [`selectVolcanoLines()`](https://pinin4fjords.github.io/shinyngs/reference/selectVolcanoLines.md)
  : Select which volcano plot threshold lines to draw

- [`selectmatrix()`](https://pinin4fjords.github.io/shinyngs/reference/selectmatrix.md)
  : The server function of the selectmatrix module

- [`selectmatrixInput()`](https://pinin4fjords.github.io/shinyngs/reference/selectmatrixInput.md)
  : The UI input function of the selectmarix module

- [`shinyngs-package`](https://pinin4fjords.github.io/shinyngs/reference/shinyngs.md)
  [`shinyngs`](https://pinin4fjords.github.io/shinyngs/reference/shinyngs.md)
  : Interactive downstream analysis with ShinyNGS.

- [`shinyngsPageNavbar()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsPageNavbar.md)
  : Build the top-level bslib page shell shared by the app modules

- [`shinyngsPlotlyConfig()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsPlotlyConfig.md)
  : Apply shinyngs' shared plotly toolbar configuration

- [`shinyngsSpinnerColor()`](https://pinin4fjords.github.io/shinyngs/reference/shinyngsSpinnerColor.md)
  : Accent colour for loading spinners

- [`simpleApp()`](https://pinin4fjords.github.io/shinyngs/reference/simpleApp.md)
  :

  Produce a simple app with controls and layout for a single module, in
  a shiny `sideBarLayout()`.

- [`simpletable()`](https://pinin4fjords.github.io/shinyngs/reference/simpletable.md)
  : The server function of the simpletable module

- [`simpletableInput()`](https://pinin4fjords.github.io/shinyngs/reference/simpletableInput.md)
  : The UI input function of the simpletable module

- [`simpletableOutput()`](https://pinin4fjords.github.io/shinyngs/reference/simpletableOutput.md)
  : The output function of the simpletable module

- [`simplifyContrastTable()`](https://pinin4fjords.github.io/shinyngs/reference/simplifyContrastTable.md)
  : Simplify a contrast table

- [`splitAnnotationLegend()`](https://pinin4fjords.github.io/shinyngs/reference/splitAnnotationLegend.md)
  : Replace heatmaply's combined annotation legend with one split by
  variable

- [`summarisematrix()`](https://pinin4fjords.github.io/shinyngs/reference/summarisematrix.md)
  : The server function of the summarisematrix module

- [`summarisematrixInput()`](https://pinin4fjords.github.io/shinyngs/reference/summarisematrixInput.md)
  : The input function of the summarizematrix module

- [`summaryTileSpecs()`](https://pinin4fjords.github.io/shinyngs/reference/summaryTileSpecs.md)
  : Assemble the summary tile specifications for an experiment list

- [`summaryTileTag()`](https://pinin4fjords.github.io/shinyngs/reference/summaryTileTag.md)
  : Build a single clickable summary tile

- [`summarytiles()`](https://pinin4fjords.github.io/shinyngs/reference/summarytiles.md)
  : The server function of the summarytiles module

- [`summarytilesInput()`](https://pinin4fjords.github.io/shinyngs/reference/summarytilesInput.md)
  : The UI input function of the summarytiles module

- [`summarytilesOutput()`](https://pinin4fjords.github.io/shinyngs/reference/summarytilesOutput.md)
  : The output function of the summarytiles module

- [`upset()`](https://pinin4fjords.github.io/shinyngs/reference/upset.md)
  : The server function of the upstart module

- [`upsetInput()`](https://pinin4fjords.github.io/shinyngs/reference/upsetInput.md)
  : The input function of the upset module

- [`upsetOutput()`](https://pinin4fjords.github.io/shinyngs/reference/upsetOutput.md)
  : The output function of the clustering module

- [`validateFormulaBasedContrast()`](https://pinin4fjords.github.io/shinyngs/reference/validateFormulaBasedContrast.md)
  : Validate a formula-based contrast string against fixed-effect
  coefficients

- [`volcanoplot()`](https://pinin4fjords.github.io/shinyngs/reference/volcanoplot.md)
  :

  The server function of the `volcanoplot` module

- [`volcanoplotInput()`](https://pinin4fjords.github.io/shinyngs/reference/volcanoplotInput.md)
  :

  The UI input function of the `volcanoplot` module

- [`volcanoplotOutput()`](https://pinin4fjords.github.io/shinyngs/reference/volcanoplotOutput.md)
  :

  The output function of the `volcanoplot` module
