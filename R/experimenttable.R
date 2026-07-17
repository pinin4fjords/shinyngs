experimenttable_modal <- list(id = "experimenttable", title = "Experimental data table")

#' The UI input function of the experimenttable module
#'
#' This module produces a simple table of the \code{colData()} in a
#' SummarizedExperiment object. If more than one of these objects were
#' specified, a select box should appear to pick the desired one for display.
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
#' experimenttableInput("experiment", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("experimenttable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
experimenttableInput <- function(id, eselist) {
  ns <- NS(id)

  description <- "This is the metadata associated with the experimental samples of this study."

  if (length(eselist) == 1) {
    tagList(hiddenInput(ns("experiment"), names(eselist)[1]), fieldSets(ns("fieldset"), list(export = simpletableInput(
      ns("experimenttable"), "Experiment",
      description
    ))))
  } else {
    fieldSets(ns("fieldset"), list(experiment = selectInput(ns("experiment"), "Experiment", names(eselist)), export = simpletableInput(
      ns("experimenttable"),
      "experiment", description
    )))
  }
}

#' The output function of the experimenttable module
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
#' experimenttableOutput("experiment")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("experimenttable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
experimenttableOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    "Experimental data",
    simpletableOutput(ns("experimenttable")),
    help = modalInput(ns(experimenttable_modal$id), "help", "help")
  )
}

#' The server function of the experimenttable module
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
#'   experimenttable("experimenttable", eselist)
#'   app <- prepareApp("experimenttable", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
experimenttable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(experimenttable_modal$id, experimenttable_modal$title)

    getExperiment <- reactive({
      experiment <- data.frame(colData(eselist[[input$experiment]]), check.names = FALSE)
      colnames(experiment) <- prettifyVariablename(colnames(experiment))
      experiment
    })

    simpletable("experimenttable", displayMatrix = getExperiment, filename = "experiment", rownames = TRUE)
  })
}
