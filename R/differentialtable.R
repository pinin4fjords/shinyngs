differentialtable_modal <- list(id = "differentialtable", title = "Differential expression table")

#' The UI input function of the differentialtable module
#'
#' This module provides information on the comparison betwen pairs of groups
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
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
#' differentialtableInput("experiment", eselist)
#'
differentialtableInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("expression"), eselist)
  fieldSets(ns("fieldset"), list(contrasts = list(contrastsInput(ns("differential"), dynamic_filters = TRUE)), select_assay_data = expression_filters, export = simpletableInput(ns("differentialtable"))))
}

#' The output function of the differentialtable module
#'
#' This module provides information on the comparison betwen pairs of groups
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
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
#' differentialtableOutput("experiment")
#'
differentialtableOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    NULL,
    htmlOutput(ns("differentialtable")),
    contrastsOutput(ns("differential")),
    help = modalInput(ns(differentialtable_modal$id), "help", "help")
  )
}

#' The server function of the differentialtable module
#'
#' This module provides information on the comparison betwen pairs of groups
#' defined in a 'contrasts' slot of a ExploratorySummarizedExperimentList
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' Essentially this funnction uses the \code{contrasts} module to group samples
#' and calculate fold changes, adding test statistics where available.
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' differentialtable("differentialtable", eselist)
#'
differentialtable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(differentialtable_modal$id, differentialtable_modal$title)

    # Render the output area - and provide an input-dependent title

    output$differentialtable <- renderUI({
      ns <- session$ns

      simpletableOutput(ns("differentialtable"), tabletitle = paste("Differential expression in assay", getAssay(), sep = ": "))
    })

    # Call the selectmatrix module and unpack the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = TRUE, provide_all_genes = TRUE)
    unpack.list(selectmatrix_reactives)

    # Pass the matrix to the contrasts module for processing

    unpack.list(contrasts("differential", selectmatrix_reactives = selectmatrix_reactives, eselist = eselist, multiple = TRUE))

    # Pass the matrix to the simpletable module for display

    simpletable("differentialtable", downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "differential", rownames = FALSE)
  })
}
