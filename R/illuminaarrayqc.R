illuminaarrayqc_modal <- list(id = "illuminaarrayqc", title = "Quality control plot for Illumina microarray data")

#' The input function of the illuminaarrayqc module
#'
#' This module plots control probes from an illumina microarray experiment.
#'
#' This funcion provides the form elements to control the display
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
#' library(shinyngs)
#' illuminaarrayqcInput("myid", eselist)
#'
illuminaarrayqcInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("illuminaarrayqc"), eselist[names(eselist) == "control"])

  fieldSets(ns("fieldset"), list(quality_checks = tags$ul(
    tags$li("cy3_high > cy3_med > cy3_low"), tags$li("low stringency =~ 0"), tags$li("high stringency <= cy3 high"),
    tags$li("housekeeping = high"), tags$li("biotin = high"), tags$li("negative =~ 0")
  ), `table fields` = expression_filters, export = simpletableInput(ns("qctable"))))
}

#' The output function of the illuminaarrayqc module
#'
#' This module plots control probes from an illumina microarray experiment.
#'
#' This funcion provides the form elements to control the display
#'
#' This provides actual plot element for display by applications
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' illuminaarrayqcOutput("myid")
#'
illuminaarrayqcOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    "Illumina microarray QC plot",
    shinycssloaders::withSpinner(plotlyOutput(ns("qcplot"), height = "600px"), color = shinyngsSpinnerColor()),
    h4("Table of data"),
    simpletableOutput(ns("qctable")),
    help = modalInput(ns(illuminaarrayqc_modal$id), "help", "help")
  )
}

#' The server function of the illuminaarrayqc module
#'
#' This module plots control probes from an illumina microarray experiment.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' illuminaarayqc("myid", eselist)
#'
illuminaarrayqc <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(illuminaarrayqc_modal$id, illuminaarrayqc_modal$title)

    selectmatrix_reactives <- selectmatrix("illuminaarrayqc", eselist[names(eselist) == "control"], select_genes = FALSE, select_samples = FALSE, select_assay = FALSE)

    output$qcplot <- renderPlotly({
      experiment <- selectmatrix_reactives$selectColData()
      control_annotation <- selectmatrix_reactives$getAnnotation()
      controls <- selectmatrix_reactives$selectMatrix()

      interactive_illumina_control_probes(control_annotation, controls, sample_order = rownames(experiment)) %>%
        shinyngsPlotlyConfig("array_qc", format = session$userData$plotFormat())
    })

    # Render the table and provide for download, using the simpletable module.

    simpletable("qctable", downloadMatrix = selectmatrix_reactives$selectLabelledMatrix, displayMatrix = selectmatrix_reactives$selectLabelledLinkedMatrix, filename = "illumina_array_qc", rownames = FALSE)
  })
}

#' Make an Illumina microarray control-probe QC plot with \code{plot_ly()}
#'
#' Averages a fixed set of Illumina control probe groups (cy3 dilution
#' series, stringency, negative, biotin, labeling and housekeeping controls)
#' across samples and plots them as lines, following the standard QC
#' relationships between groups (see \code{\link{illuminaarrayqcInput}}'s
#' displayed checklist: cy3_high > cy3_med > cy3_low, low stringency =~ 0,
#' high stringency <= cy3 high, housekeeping/biotin = high, negative =~ 0).
#'
#' @param control_annotation Data frame of control probe annotation, with an
#'   \code{Array_Address_Id} column matching the row names of \code{controls}
#'   and a \code{Reporter_Group_id} column identifying each control probe
#'   group
#' @param controls Matrix of control probe intensities, control probes (by
#'   \code{Array_Address_Id}) by row, samples by column
#' @param sample_order Character vector giving the sample (x axis) display
#'   order. Defaults to \code{colnames(controls)}
#'
#' @return output A plotly htmlwidget
#'
#' @export
#'
#' @examples
#' control_annotation <- data.frame(
#'   Array_Address_Id = paste0("probe", 1:9),
#'   Reporter_Group_id = c(
#'     "phage_lambda_genome:low", "phage_lambda_genome:med", "phage_lambda_genome:high",
#'     "phage_lambda_genome:pm", "phage_lambda_genome:mm2", "permuted_negative",
#'     "phage_lambda_genome", "thrB", "housekeeping"
#'   )
#' )
#' controls <- matrix(runif(9 * 3, 1, 1000),
#'   nrow = 9, dimnames = list(control_annotation$Array_Address_Id, paste0("sample", 1:3))
#' )
#'
#' interactive_illumina_control_probes(control_annotation, controls)
#'
interactive_illumina_control_probes <- function(control_annotation, controls, sample_order = NULL) {
  if (is.null(sample_order)) {
    sample_order <- colnames(controls)
  }

  controls_merged <- merge(control_annotation, controls, by.x = "Array_Address_Id", by.y = "row.names")

  qc_groups <- list(
    cy3_low = "phage_lambda_genome:low", cy3_med = "phage_lambda_genome:med", cy3_high = "phage_lambda_genome:high", low_stringency_pm = "phage_lambda_genome:pm",
    low_stringency_mm = "phage_lambda_genome:mm2", negative = "permuted_negative", biotin = "phage_lambda_genome", labeling = "thrB", housekeeping = "housekeeping"
  )

  plotdata <- melt_matrix(do.call(cbind, lapply(qc_groups, function(qcg) {
    colMeans(controls_merged[grep(paste0(qcg, "($|,)"), controls_merged$Reporter_Group_id), colnames(controls)])
  })))

  dplyr::group_by(plotdata, Var2) %>%
    plot_ly() %>%
    add_lines(x = ~Var1, y = ~value, color = ~Var2, colors = c(
      "red", "red", "red", "orange", "orange", "black",
      "purple", "blue", "green"
    ), linetype = ~Var2, linetypes = c("dot", "dash", "solid", "dash", "solid", "solid", "solid", "solid", "solid")) %>%
    layout(xaxis = list(
      categoryarray = sample_order,
      categoryorder = "array", title = ""
    ), yaxis = list(title = "Intensity"), margin = list(b = 200))
}
