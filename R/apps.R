#' Make UI and server functions for Shiny apps based on data supplied as
#' modfied SummarizedExperiments
#'
#' Draws on various components (heatmaps, tables etc) to produce the UI and server
#' components of a variety of shiny apps, based on the type and data specified,
#' using modularised Shiny components.
#'
#' @param type A string specifying the type of shiny app required. Currently,
#'   'rnaseq' or 'chipseq' produce large multi-panel applications designed to
#'   facilitate analysis of those data types. For any other 'type', the call is
#'   passed to \code{simpleApp()}, which will attempt to build an application
#'   using \code{Input} and \code{Output} functions of the same module.
#' @param eselist An ExploratorySummarizedExperimentList object containing assay
#'   data (expression, counts...), sample data and annotation data for the rows.
#' @param ui_only Don't add server components (for UI testing)
#' @param ... Additional arguments passed to \code{simpleApp()}
#'
#' @return output A list of length 2 containing: the UI and server components
#'
#' @keywords shiny
#'
#' @import shiny
#' @import plotly
#' @export
#'
#' @examples
#' library(shinyngs)
#'
#' # 1: BASIC RNA-SEQ APP
#'
#' # Get an example RNA-seq dataset from the `airway` package
#'
#' data(airway, package = "airway")
#'
#' # Get some information about these data from the package description
#'
#' expinfo <- packageDescription("airway")
#'
#' # Convert to an ExploratorySummarizedExperiment (with extra slots)
#'
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#'
#' # Make the ExploratorySummarizedExperimentList that represents the study as a
#' # whole.
#'
#' eselist <- ExploratorySummarizedExperimentList(
#'   ese,
#'   title = expinfo$Title,
#'   author = expinfo$Author,
#'   description = expinfo$Description
#' )
#'
#' # Make and run the app
#'
#' if (interactive()) {
#'   app <- prepareApp("rnaseq", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
#' # 2: AUGMENT WITH ANNOTATION INFO FOR MORE INFORMATIVE APP
#'
#' # Use Biomart to retrieve some annotation, add it to the object, then choose a
#' # different default grouping variable and re-make the app.
#'
#' if (interactive()) {
#'   library(biomaRt)
#'   attributes <- c(
#'     "ensembl_gene_id", # The sort of ID your results are keyed by
#'     "entrezgene", # Could be used for gene sets keyed by Entrez ID
#'     "external_gene_name" # Used to annotate gene names on the plot
#'   )
#'
#'   mart <- useMart(
#'     biomart = "ENSEMBL_MART_ENSEMBL",
#'     dataset = "hsapiens_gene_ensembl", host = "www.ensembl.org"
#'   )
#'   annotation <- getBM(attributes = attributes, mart = mart)
#'   annotation <- annotation[order(annotation$entrezgene), ]
#'
#'   mcols(ese) <- annotation[match(rownames(ese), annotation$ensembl_gene_id), ]
#'   ese@labelfield <- "external_gene_name"
#'
#'   eselist <- ExploratorySummarizedExperimentList(
#'     ese,
#'     title = expinfo$Title,
#'     author = expinfo$Author,
#'     description = expinfo$Description,
#'     default_groupvar = "dex"
#'   )
#'   app <- prepareApp("rnaseq", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
#' # 3. MORE COMPLEX DATA FOR DIFFERENTIAL EXPRESSION ETC
#'
#' # See the vignette for how to populate the extra slots of an
#' # ExploratorySummarizedExperimentList (contrasts, differential statistics,
#' # gene set analyses etc.). With those in place the resulting app gains
#' # additional panels for differential analyses.
prepareApp <- function(type, eselist, ui_only = FALSE, ...) {
  if (type %in% c("rnaseq", "chipseq", "illuminaarray")) {
    inputFunc <- get(paste0(type, "Input"))

    app <- list(ui = inputFunc(type, eselist), server = function(input, output, session) {
      get(type)(type, eselist)
    })
  } else {
    app <- simpleApp(eselist, type, ui_only = ui_only, ...)
  }

  # Preload htmlwidget JS/CSS as <script>/<link> tags in the initial HTML so
  # plugins like jQuery DataTables are attached at page-load. Otherwise
  # htmlwidgets 1.6.4's shinyBinding.renderValue calls the synchronous
  # Shiny.renderDependencies() and immediately runs bindingDef.renderValue
  # (e.g. $().DataTable). The plugin <script> is appended to <head> but not
  # awaited. Locally that's a <1ms race; behind a reverse proxy (Seqera
  # Studios, Shiny Server behind nginx, etc.) it consistently loses, surfacing
  # as "$table.DataTable is not a function" and empty plots/tables.
  app$ui <- htmltools::attachDependencies(
    app$ui,
    htmlwidget_preload_deps(),
    append = TRUE
  )

  app
}

# Dependencies for every htmlwidget type used anywhere in shinyngs, so they
# load as blocking <script>/<link> tags in the initial HTML rather than lazily
# via WebSocket-delivered dep messages. See prepareApp() for rationale.
htmlwidget_preload_deps <- function() {
  htmltools::resolveDependencies(c(
    htmltools::findDependencies(DT::datatable(data.frame(a = 1))),
    htmltools::findDependencies(plotly::plot_ly(x = 1, y = 1))
  ))
}

#' Produce a simple app with controls and layout for a single module, in a
#' shiny \code{sideBarLayout()}.
#'
#' Internal function called by prepareApp() to make simple single-function
#' apps.
#'
#' @param eselist List of ExploratorySummarizedExperiment objects with assay and experimental
#' data
#' @param module Character string specifying the module to use
#' @param ui_only Don't add server components (for UI testing)
#' @param ... Additional arguments passed to the module output function
#'
#' @keywords shiny
#'
#' @examples
#' simpleApp(eselist, "heatmap", "My study name")
#'
simpleApp <- function(eselist, module = NULL, ui_only = FALSE, ...) {
  inputFunc <- get(paste0(module, "Input"))
  outputFunc <- get(paste0(module, "Output"))

  moduletitle <- prettifyVariablename(module)

  if (!is.null(module)) {
    ui <- shinyngsPageNavbar(list(
      id = "pages", title = moduletitle,
      window_title = moduletitle,
      bslib::nav_panel(prettifyVariablename(module), sidebarLayout(sidebarPanel(inputFunc(module, eselist, ...), width = 3), mainPanel(outputFunc(
        module,
        ...
      ), width = 9)))
    ))

    if (ui_only) {
      server <- function(input, output, session) {
      }
    } else {
      server <- function(input, output, session) {
        get(module)(module, eselist, ...)
      }
    }
    list(ui = ui, server = server)
  }
}
