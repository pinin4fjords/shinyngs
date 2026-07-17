assaydatatable_modal <- list(id = "assaydatatable", title = "Assay data table")

#' The UI input function of the assaydatatable module
#'
#' This module displays the content of the currently selected experiment and
#' assay, also allowing grouping by mean etc.
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
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' assaydatatableInput("experiment", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("assaydatatable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
assaydatatableInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("expression"), eselist)
  fieldSets(ns("fieldset"), list(select_assay_data = expression_filters, export = simpletableInput(ns("assaydatatable"))))
}

#' The output function of the assaydatatable module
#'
#' This module displays the content of the currently selected experiment and
#' assay, also allowing grouping by mean etc.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' assaydatatableOutput("experiment")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("assaydatatable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
assaydatatableOutput <- function(id) {
  ns <- NS(id)

  moduleCard(
    "Assay data",
    htmlOutput(ns("assaydatatable")),
    help = modalInput(ns(assaydatatable_modal$id), "help", "help")
  )
}

#' The server function of the assaydatatable module
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
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   assaydatatable("assaydatatable", eselist)
#'   app <- prepareApp("assaydatatable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
assaydatatable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(assaydatatable_modal$id, assaydatatable_modal$title)

    # Render the output area - and provide an input-dependent title

    output$assaydatatable <- renderUI({
      ns <- session$ns

      simpletableOutput(ns("assaydatatable"), tabletitle = paste("Assay data", getAssay(), sep = ": "))
    })

    # Call the selectmatrix module and unpack the reactives it sends back

    unpack.list(selectmatrix("expression", eselist, var_n = 1000, select_genes = TRUE, provide_all_genes = TRUE))

    # Pass the matrix to the simpletable module for display

    simpletable("assaydatatable", downloadMatrix = selectLabelledMatrix, displayMatrix = selectLabelledLinkedMatrix, filename = getAssay(), rownames = FALSE)
  })
}
