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
#' @export

readreportsInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    fieldSets(ns("fieldsets"), list(report_type = uiOutput(ns("reportType")), bar_plot = uiOutput(ns("barplotControls")), export = simpletableInput(ns("readrep"))))
    
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
#' @export

readreportsOutput <- function(id, eselist) {
    ns <- NS(id)
    
    list(modalInput(ns("readreports"), "help", "help"), modalOutput(ns("readreports"), "Read reports", includeMarkdown(system.file("inlinehelp", 
        "readreports.md", package = packageName()))), uiOutput(ns("plotTitle")), uiOutput(ns("barplotOutput")), uiOutput(ns("tableTitle")), 
        simpletableOutput(ns("readrep")))
}

#' Server function of the \code{readreports} module 
#' 
#' Display plots and tables relating to read mapping, attrition during 
#' analysis etc.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperiment with \code{read_distribution}
#' slot filled
#'
#' @export

readreports <- function(input, output, session, eselist) {
    
    ns <- session$ns
    
    # Render a select for the report type based on what's in the 'read_reports' slot
    
    output$reportType <- renderUI({
        selectInput(ns("reportType"), "Report type", structure(names(eselist@read_reports), names = prettifyVariablename(names(eselist@read_reports))))
    })
    
    # Render bar plot controls with default behaviour dependent on report type
    
    output$barplotControls <- renderUI({
        barplotInput(ns("barplot"), default_mode = getDefaultMode())
    })
    
    # Render the bar plot with height dependent on the number of columns in the report (so that the legend fits)
    
    output$barplotOutput <- renderUI({
        min_height <- 400
        height <- max(min_height, ncol(eselist@read_reports[[getReportType()]]) * 20)
        barplotOutput(ns("barplot"), height)
    })
    
    # Dynamic title for the plot
    
    output$plotTitle <- renderUI({
        h3(paste(prettifyVariablename(getReportType()), "plot"))
    })
    
    # Dynamic title for the table
    
    output$tableTitle <- renderUI({
        h4(paste(prettifyVariablename(getReportType()), "data"))
    })
    
    # Return the selected plot type when available
    
    getReportType <- reactive({
        validate(need(input$reportType, FALSE))
        input$reportType
    })
    
    # Choose a default bar mode based on the report type. For read attrition when the counts at each analysis stage are a subset of those at
    # the previous, it makes sense to use overlapped bars.
    
    getDefaultMode <- reactive({
        if (getReportType() == "read_attrition") {
            "overlay"
        } else {
            "stack"
        }
    })
    
    # The barplot module expects data in columns by sample, so transform the table.
    
    getPlotmatrix <- reactive({
        t(getReportTable())
    })
    
    # Get the report table from the slot
    
    getReportTable <- reactive({
        plotmatrix <- eselist@read_reports[[getReportType()]]
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
