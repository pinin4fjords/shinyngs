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
#'   app <- prepare_app("rnaseq", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
#' # 2: AUGMENT WITH ANNOTATION INFO FOR MORE INFORMATIVE APP
#'
#' # Add row metadata (gene symbol, genomic coordinates) so plots and tables
#' # are labelled meaningfully, then choose a different default grouping
#' # variable and re-make the app. Any source of per-gene annotation works
#' # here (a GTF, an annotation package, a pre-built CSV); the requirement is
#' # a data.frame keyed by the same IDs as rownames(ese). Adding
#' # chromosome_name/start_position/end_position columns plus ensembl_species
#' # also enables the igv.js gene model view on the gene page.
#'
#' if (interactive()) {
#'   annotation <- data.frame(
#'     ensembl_gene_id = rownames(ese),
#'     external_gene_name = rownames(ese), # substitute real gene symbols
#'     chromosome_name = "1", # substitute real coordinates
#'     start_position = seq_along(rownames(ese)),
#'     end_position = seq_along(rownames(ese)) + 1000
#'   )
#'
#'   mcols(ese) <- annotation[match(rownames(ese), annotation$ensembl_gene_id), ]
#'   ese@labelfield <- "external_gene_name"
#'
#'   eselist <- ExploratorySummarizedExperimentList(
#'     ese,
#'     title = expinfo$Title,
#'     author = expinfo$Author,
#'     description = expinfo$Description,
#'     default_groupvar = "dex",
#'     ensembl_species = "hsapiens" # enables the igv.js gene model view
#'   )
#'   app <- prepare_app("rnaseq", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
#' # 3. MORE COMPLEX DATA FOR DIFFERENTIAL EXPRESSION ETC
#'
#' # See the vignette for how to populate the extra slots of an
#' # ExploratorySummarizedExperimentList (contrasts, differential statistics,
#' # gene set analyses etc.). With those in place the resulting app gains
#' # additional panels for differential analyses.
prepare_app <- function(type, eselist, ui_only = FALSE, ...) {
  # URL bookmarking lets a configured view be captured in a shareable link and
  # restored. Set here (rather than via shinyApp(enableBookmarking=)) because
  # callers invoke shiny::shinyApp()/runApp() themselves, so this is the only
  # place the package controls.
  shiny::enableBookmarking("url")

  big <- type %in% c("rnaseq", "chipseq", "illuminaarray")

  # The UI is built afresh on every request so that the restoreInput() calls
  # inside Shiny input constructors run within the restore context of a
  # bookmark load and pick up the saved values. A UI built once, ahead of any
  # request, would bake in defaults and never restore.
  #
  # attachDependencies preloads htmlwidget JS/CSS as <script>/<link> tags in the
  # initial HTML so plugins like jQuery DataTables are attached at page-load.
  # Otherwise htmlwidgets 1.6.4's shinyBinding.renderValue calls the synchronous
  # Shiny.renderDependencies() and immediately runs bindingDef.renderValue (e.g.
  # $().DataTable). The plugin <script> is appended to <head> but not awaited.
  # Locally that's a <1ms race; behind a reverse proxy (Seqera Studios, Shiny
  # Server behind nginx, etc.) it consistently loses, surfacing as
  # "$table.DataTable is not a function" and empty plots/tables.
  buildUI <- function() {
    ui <- if (big) {
      get(paste0(type, "Input"))(type, eselist)
    } else {
      simpleApp(eselist, type, ui_only = ui_only, ...)$ui
    }
    htmltools::attachDependencies(ui, htmlwidget_preload_deps(), append = TRUE)
  }

  server <- if (big) {
    function(input, output, session) {
      configureBookmarking(input, session, nav_input = paste0(type, "-", type))
      get(type)(type, eselist)
    }
  } else {
    simpleApp(eselist, type, ui_only = ui_only, ...)$server
  }

  list(ui = function(request) buildUI(), server = server)
}

# Dependencies for every htmlwidget type used anywhere in shinyngs, so they
# load as blocking <script>/<link> tags in the initial HTML rather than lazily
# via WebSocket-delivered dep messages. See prepare_app() for rationale. The set
# is constant, so it is computed once and cached (buildUI() runs per page-load).
htmlwidget_preload_deps <- local({
  cached <- NULL
  function() {
    if (is.null(cached)) {
      cached <<- htmltools::resolveDependencies(c(
        htmltools::findDependencies(DT::datatable(data.frame(a = 1))),
        htmltools::findDependencies(plotly::plot_ly(x = 1, y = 1))
      ))
    }
    cached
  }
})

#' Produce a simple app with controls and layout for a single module, in a
#' shiny \code{sideBarLayout()}.
#'
#' Internal function called by prepare_app() to make simple single-function
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

  moduletitle <- prettify_variable_name(module)

  if (!is.null(module)) {
    ui <- shinyngsPageNavbar(list(
      id = "pages", title = moduletitle,
      window_title = moduletitle,
      bslib::nav_panel(prettify_variable_name(module), moduleLayout(inputFunc(module, eselist, ...), outputFunc(module, ...)))
    ))

    if (ui_only) {
      server <- function(input, output, session) {
      }
    } else {
      server <- function(input, output, session) {
        configureBookmarking(input, session, nav_input = "pages")
        get(module)(module, eselist, ...)
      }
    }
    list(ui = ui, server = server)
  }
}
