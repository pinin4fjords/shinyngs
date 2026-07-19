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

  fieldset_list <- list(
    export = simpletableInput(ns("experimenttable"), "Experiment", description),
    category_counts = categorycountplotInput(ns("categorycount"))
  )

  if (length(eselist) == 1) {
    tagList(hiddenInput(ns("experiment"), names(eselist)[1]), fieldSets(ns("fieldset"), fieldset_list))
  } else {
    fieldSets(ns("fieldset"), c(list(experiment = selectInput(ns("experiment"), "Experiment", names(eselist))), fieldset_list))
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
    tabsetPanel(
      tabPanel("Table", simpletableOutput(ns("experimenttable"))),
      tabPanel("Category counts", categorycountplotOutput(ns("categorycount")))
    ),
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

    getRawExperiment <- reactive({
      data.frame(colData(eselist[[input$experiment]]), check.names = FALSE)
    })

    getExperiment <- reactive({
      experiment <- getRawExperiment()
      colnames(experiment) <- prettifyVariablename(colnames(experiment))
      experiment
    })

    simpletable("experimenttable", displayMatrix = getExperiment, filename = "experiment", rownames = TRUE)

    categorycountplot("categorycount", getAnnotation = getRawExperiment, filename = "experiment_categorycounts")
  })
}
