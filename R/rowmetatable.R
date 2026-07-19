rowmetatable_modal <- list(id = "rowmetatable", title = "Experimental data table")

#' The UI input function of the rowmetatable module
#'
#' This module produces a simple table of the row metadata (accessed via
#' \code{mcols}) in a SummarizedExperiment object. If more than one of these
#' objects were specified, a select box should appear to pick the desired one for display.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' experimentableInput("experiment", eselist)
#'
rowmetatableInput <- function(id, eselist) {
  ns <- NS(id)

  description <- "This is the metadata associated with the rows (e.g. genes) of this study."

  list(
    selectmatrixInput(ns("rowmetatable"), eselist = eselist),
    simpletableInput(ns("rowmetatable"), tabletitle = "Annotation"),
    categorycountplotInput(ns("categorycount"))
  )
}

#' The output function of the rowmetatable module
#'
#' This module produces a simple table of the \code{colData()} in an
#' ExploratorySummarizedExperiment object. If more than one of these objects
#' were specified, a select box should appear to pick the desired one for
#' display.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' rowmetatableOutput("experiment")
#'
rowmetatableOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    "Row metadata",
    tabsetPanel(
      tabPanel("Table", simpletableOutput(ns("rowmetatable"))),
      tabPanel("Category counts", categorycountplotOutput(ns("categorycount")))
    ),
    help = modalInput(ns(rowmetatable_modal$id), "help", "help")
  )
}

#' The server function of the rowmetatable module
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example). Essentially this just passes the results of \code{colData()}
#' applied to the specified SummarizedExperiment object to the
#' \code{simpletable} module
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' rowmetatable("rowmetatable", eselist)
#'
rowmetatable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(rowmetatable_modal$id, rowmetatable_modal$title)

    selectmatrix_reactives <- selectmatrix("rowmetatable", eselist, select_assays = FALSE, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE)

    getRowMeta <- reactive({
      meta <- selectmatrix_reactives$getAnnotation()
      meta
    })

    getLinkedRowMeta <- reactive({
      meta <- getRowMeta()
      colnames(meta) <- prettify_variable_name(colnames(meta))
      linkMatrix(meta, eselist@url_roots)
    })

    simpletable("rowmetatable", displayMatrix = getLinkedRowMeta, downloadMatrix = getRowMeta, filename = "rowmeta", rownames = TRUE, pageLength = 10)

    categorycountplot("categorycount", getAnnotation = getRowMeta, filename = "rowmeta_categorycounts")
  })
}
