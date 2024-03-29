#' Input function of the \code{readreports} module
#'
#' Display plots and tables relating to read mapping, attrition during
#' analysis etc.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of controls that can be added to a UI definition

readreportsInput <- function(id, eselist) {
  ns <- NS(id)

  naked_fields <- list()
  field_sets <- list()

  eselist <- eselist[unlist(lapply(eselist, function(x) length(x@read_reports) > 0))]
  experiment_filter <- selectmatrixInput(ns("readreports"), eselist = eselist)

  if (length(eselist) > 1) {
    field_sets$experiment <- experiment_filter
  } else {
    naked_fields <- experiment_filter
  }

  field_sets <- c(field_sets, list(report_type = uiOutput(ns("reportType")), bar_plot = uiOutput(ns("barplotControls")), export = simpletableInput(ns("readrep"))))

  list(naked_fields, fieldSets(ns("fieldsets"), field_sets))
}

#' Output function of the \code{readreports} module
#'
#' Display plots and tables relating to read mapping, attrition during
#' analysis etc.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @return A list of elements that can be included in a panel

readreportsOutput <- function(id, eselist) {
  ns <- NS(id)

  list(modalInput(ns("readreports"), "help", "help"), modalOutput(ns("readreports"), "Read reports", includeMarkdown(system.file("inlinehelp", "readreports.md",
    package = packageName()
  ))), uiOutput(ns("plotTitle")), uiOutput(ns("barplotOutput")), uiOutput(ns("tableTitle")), simpletableOutput(ns("readrep")))
}

#' Server function of the \code{readreports} module
#'
#' Display plots and tables relating to read mapping, attrition during
#' analysis etc.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled

readreports <- function(input, output, session, eselist) {
  ns <- session$ns

  unpack.list(callModule(selectmatrix, "readreports", eselist, select_assays = FALSE, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE))

  # Render a select for the report type based on what's in the 'read_reports' slot

  output$reportType <- renderUI({
    ese <- getExperiment()
    selectInput(ns("reportType"), "Report type", structure(names(ese@read_reports), names = prettifyVariablename(names(ese@read_reports))))
  })

  # Render bar plot controls with default behaviour dependent on report type

  output$barplotControls <- renderUI({
    default_mode <- getDefaultMode()
    barplotInput(ns("barplot"), default_mode = default_mode)
  })

  # Render the bar plot with height dependent on the number of columns in the report (so that the legend fits)

  output$barplotOutput <- renderUI({
    ese <- getExperiment()
    report_type <- getReportType()
    min_height <- 400
    height <- max(min_height, ncol(ese@read_reports[[report_type]]) * 20)
    barplotOutput(ns("barplot"), height)
  })

  # Dynamic title for the plot

  output$plotTitle <- renderUI({
    report_type <- getReportType()
    h3(paste(prettifyVariablename(report_type), "plot"))
  })

  # Dynamic title for the table

  output$tableTitle <- renderUI({
    report_type <- getReportType()
    h4(paste(prettifyVariablename(report_type), "data"))
  })

  # Return the selected plot type when available

  getReportType <- reactive({
    validate(need(input$reportType, FALSE))
    input$reportType
  })

  # Choose a default bar mode based on the report type. For read attrition when the counts at each analysis stage are a subset of those at the previous, it
  # makes sense to use overlapped bars.

  getDefaultMode <- reactive({
    report_type <- getReportType()
    if (report_type == "read_attrition") {
      "overlay"
    } else {
      "stack"
    }
  })

  # The barplot module expects data in columns by sample, so transform the table.

  getPlotmatrix <- reactive({
    report_table <- getReportTable()
    t(report_table)
  })

  # Get the report table from the slot

  getReportTable <- reactive({
    ese <- getExperiment()
    report_type <- getReportType()
    plotmatrix <- ese@read_reports[[report_type]]
    plotmatrix <- plotmatrix[, apply(plotmatrix, 2, function(x) sum(x > 10) > 0)]
    plotmatrix <- plotmatrix[, order(colMeans(plotmatrix), decreasing = TRUE)]
    colnames(plotmatrix) <- prettifyVariablename(colnames(plotmatrix))
    plotmatrix
  })

  # Call the modules to produce the plot and table

  callModule(barplot, "barplot", getPlotmatrix = getPlotmatrix, getYLabel = reactive({
    "Reads"
  }))
  callModule(simpletable, "readrep", displayMatrix = getReportTable, filename = "read_report", rownames = TRUE)
}
