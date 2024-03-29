#' Interactive downstream analysis with ShinyNGS.
#'
#' shinyngs is a package for downstream visualisation of data stored in
#' \code{SummarizedExperiment}-like objects, produced as a end-point of
#' bioinformatics analysis workflows.
#'
#' What follows is technical documentation for the package, use
#' \code{vignette('shinyngs')} to get more user-friendly information on package
#' usage.
#'
#' @section Principal software dependencies: This package makes use of many
#'   packages, and is indebted to their developers.
#'   \href{http://shiny.rstudio.com/}{Shiny} provides the overall framework that
#'   made the current work possible, and \href{https://plot.ly/}{Plotly} has
#'   been key to making the included visualisations as dynamic and useful as
#'   they are.
#'
#'   Plotly's R interface has changed markedly between versions, and
#'   \code{shinyngs} is currently dependent on a version later than the (now
#'   somewhat old) version on CRAN. This should therefore be installed from
#'   GitHub: \code{devtools::install_github('ropensci/plotly',
#'   upgrade_dependencies = FALSE)}.
#'
#' @section Input data structure: \code{shinyngs} defines two new data
#'   structures. The first, \code{\link{ExploratorySummarizedExperiment}} simply
#'   takes SummarizedExperiment, and adds some slots to allow extra description
#'   around experiments. These extra slots are not compulsory, however, and a
#'   simple SummarizedExperiment can be converted directly into an
#'   \code{ExploratorySummarizedExperiment}:
#'
#'   \preformatted{ ese <- as(airway, 'ExploratorySummarizedExperiment') }
#'
#'   Because a single \code{ExploratorySummarizedExperiment} is limited to one
#'   set of features, \code{\link{ExploratorySummarizedExperimentList}} was
#'   devised to allow incorporation of other feature sets. For example, we might
#'   want to visualise expression in terms of both transcripts and genes.
#'   \code{ExploratorySummarizedExperimentList} is just a list of
#'   \code{ExploratorySummarizedExperiment}s, with some additonal slots
#'   pertaining to a study as a whole.
#'
#'   To use \code{shinyngs}, data is assumed to be in these formats. I'm not
#'   terribly happy with the structure of some of the slots in these objects,
#'   and they may be open to 'improvement' in the future.
#'
#'   \subsection{Example data}{An example based on the Zhang et al study of
#'   neurons and glia (http://www.jneurosci.org/content/34/36/11929.long) is
#'   included in the package, and can be picked apart to further understand the
#'   data structure.}
#'
#' @section Shiny modules: The functionality of \code{shinyngs} revolves around
#'   \href{http://shiny.rstudio.com/}{Shiny}'s concept of modules
#'   (\url{http://shiny.rstudio.com/articles/modules.html}), so this bears some
#'   explanation.
#'
#'   Modules allow essentially object-oriented coding for Shiny, and the hiving
#'   of of code into namespaces such that it can be re-used in different parts
#'   of an application without interfering with itself. \code{shinyngs} has many
#'   modules that provide various functionalities.
#'
#'   \subsection{Components}{Each module will have server and UI functions that
#'   provide processing and UI functionality respectively. For example, the
#'   \code{dendro} module, which provides sample clustering dengrograms, has
#'   \code{\link{dendroInput}}, which provides user interface components to
#'   determine clustering method etc, and \code{\link{dendro}}, which parses
#'   those inputs and calls the appropriate functions to generate a dendrogram.
#'   \code{\link{dendroOutput}} provides elements that display the outputs when
#'   they're ready. These functions are all present in dendro.R.}
#'
#'   \subsection{Usage}{Any application seeking to create dendrograms need only
#'   call \code{\link{dendroInput}} \code{\link{dendroOutput}} in the parts of
#'   their application where inputs and outputs should appear, and then use
#'   \code{\link[shiny]{callModule}} (again using the same ID) to activate them.
#'
#'   \preformatted{
#'   dendroInput(ns('dendro'), eselist)
#'   dendroOutput(ns('dendro'))
#'   callModule(dendro, 'dendro', eselist) }}
#'
#'   These module functions are not currently exported, not currenlty being
#'   intended for use outside \code{shinyngs}. This may change.
#'
#' @section Main functions:
#'
#'   Usage of modules defined in \code{shinyngs} is envisaged to be
#'   predominantly via \code{\link{prepareApp}}. For most modules, this passes
#'   its arguments to \code{\link{simpleApp}}, which very simply takes its
#'   \code{ExploratorySummarizedExperimentList} and a module name, and attempts
#'   to call the UI and server functions of that module, creating a simple
#'   sidebar layout. Taking our dendro app and using the \code{airway} example
#'   data:
#'
#'   \preformatted{
#'   data(airway, package = 'airway')
#'   ese <- as(airway, 'ExploratorySummarizedExperiment')
#'   eselist <- ExploratorySummarizedExperimentList(ese)
#'   app <- prepareApp('heatmap',eselist)}
#'
#'   Other apps, for example \code{\link{rnaseq}} require more complex layouts
#'   and don't use \code{simpleApp}.
#'
#'   \code{prepareApp} returns list with UI and server applications components.
#'   Shiny's \code{\link[shiny]{shinyApp}} must then be called to actually run
#'   the app: \code{shiny::shinyApp(ui = app$ui, server = app$server)}.
#'
#' @section Deploying to a shiny server:
#'
#'   Assuming a \code{ExploratorySummarizedExperimentList} has been serialised
#'   with \code{link[base]{saveRDS}}, the Shiny app can be deployed to a server
#'   in a script called 'app.R' with content like this:
#'
#'   \preformatted{
#'   library(shinyngs)
#'   mydata <- readRDS('data.rds') app <- prepareApp('rnaseq', mydata)
#'   shiny::shinyApp(app$ui, app$server)}
#'
#' @section Module list:
#'
#' There are three varieties of module in \code{shinyngs}. Top-level modules
#' such as \code{rnaseq} provide complex data mining tools pulling together a
#' number of other modules. Some of those sub-modules can also be called
#' stand-alone to make heatmaps or dendrograms, for example. Other modules just
#' provide small components that don't make sense in isolation. This is the full
#' set at the time of writing:
#'
#' \subsection{Top-level}{\describe{\item{rnaseq}{Pull together PCA analysis,
#' heat maps, dendgrograms, volcano plots, gene sets etc to make a comprehensive
#' data mining tool.} \item{chipseq}{Currenlty a near clone of \code{rnaseq}, to
#' be optimised for ChIP-seq in due course.} }}
#'
#' \subsection{Stand-alone}{
#'
#' These modules can also be called directly with \code{\link{prepareApp}}:
#'
#' \describe{ \item{assaydatatable}{Provide access to asay matrices of input
#' object} \item{boxplot}{Make quartile/ boxplots} \item{dendro}{Make
#' sample-clustering dendgrograms.} \item{dexseqplot}{Differential exon usage
#' plot (where relevant slot populated)} \item{dexseqtable}{Differential exon
#' usage table (where relevant slot populated)}
#' \item{differentialtable}{Differential table showing summarised group values
#' and fold changes etc (where contrasts slot is populated).}
#' \item{experimentaltable}{Display colData() output from selected
#' \code{ExploratorySummarizedExperiment}} \item{foldchangeplot}{Plot mean
#' values of one condition against another (where contrasts slot is populated)}
#' \item{gene}{Show expression plots and other available data for selected
#' features} \item{genesetanalysistable}{Display content of the
#' \code{gene_set_analyses} slot for the selected
#' \code{ExploratorySummarizedExperiment}} \item{genesetbarcodeplot}{Make a
#' barcode plot using selected data and \code{limma}'s
#' \code{\link[limma]{barcodeplot}}} \item{heatmap}{Build heatmaps from selected
#' matrix data. By default this is an expression matrix, but it can also be
#' sample vs sample correlation heatmaps or PCA vs covariate association
#' heatmaps.} \item{maplot}{Uses the \code{scatterplot} module to plot mean
#' expression vs fold change} \item{pca}{Plots 2D or 3D PCA and loading plots
#' based on selected matrix data, using the \code{scatterplot} module}
#' \item{readreports}{Makes bar plots and tables of read counts data where
#' provided in the \code{read_reports} slot of an
#' \code{ExploratorySummarizedExperimentList}} \item{rowmetatable}{Displays data
#' from the metadata slot of a \code{ExploratorySummarizedExperiment} object
#' (accessed via \code{mcols})} \item{volcanoplot}{Uses the \code{scatterplot}
#' module to plot fold change vs p value (where necessary slots are populated}}}
#'
#' \subsection{Component-only}{These modules are only valid as components of
#' applications created by other modules:
#'
#' \describe{ \item{barplot}{Used by readreports, for example. Requires reactive
#' providing input data} \item{contrasts}{Provides data on sample group
#' comparisons, using the \code{contrasts} slot of the input object (where
#' available)} \item{geneselect}{Provides selection filters for assay matrix
#' rows. Used mainly by the \code{selectmatrix} module}
#' \item{genesetselect}{Uses
#' \href{http://selectize.github.io/selectize.js/}{Selectize} to provide an
#' autocomplete field to select from available gene sets (where provided in the
#' \code{gene_sets} slot)} \item{groupby}{Provides a UI element to choose from
#' the \code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA
#' etc} \item{labelselectfield}{This module provides an input which allows
#' filtering on the basis of row IDs, labels, or other data in the metadata slot
#' of an \code{ExploratorySummarizedExperiment}} \item{modal}{uses modals from
#' \href{https://ebailey78.github.io/shinyBS/}{shinyBS} to create overlaid text
#' for the current panel which displays when a link is clicked}
#' \item{plotdownload}{Provides a download button for displayed plots}
#' \item{sampleselect}{Allows a subset of columns to be selected from the
#' selected assay matrix} \item{scatterplot}{A generic module for producing
#' scatter plots using Plotly's \code{\link[plotly]{plot_ly}} method. Used by
#' multiple other modules, including \code{foldchangeplot} and
#' \code{volcanoplot}} \item{scatterplotcontrols}{A set of controls for scatter
#' plots. Separation of controls in this module allows them to be used in
#' multiple scatter plot instances. For example, both PCA and loading plots can
#' be run off the same set of controls} \item{selectmatrix}{Provides
#' functionality central to \code{shinyngs}. Uses filtes on experiment and
#' assay, along with \code{sampleselect} and \code{geneselect}, to select parts
#' of input matrices for use by most other \code{shinyngs} modules}
#' \item{simpletable}{A useful generic module that takes a reactive which
#' returns a matrix, and both displays that table and produces a download
#' button} \item{summarizematrix}{Module which takes a matrix and a factor which
#' groups columns, and summarises by a supplied method, mean by default} }}
#'
#' @docType package
#' @name shinyngs

NULL
