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
    
    fieldSets(ns("fieldsets"), list(report_type = uiOutput(ns('reportType')), bar_plot = uiOutput(ns('barplotControls')), export = simpletableInput(ns("readrep"))))
    
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
        "readreports.md", package = packageName()))), uiOutput(ns('plotTitle')), uiOutput(ns('barplotOutput')), uiOutput(ns('tableTitle')), 
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
  
    output$reportType <- renderUI({
      selectInput(ns('reportType'), 'Report type', structure(names(eselist@read_reports), names = prettifyVariablename(names(eselist@read_reports))))
    })
    
    output$barplotControls <- renderUI({
      barplotInput(ns("barplot"), default_mode = getDefaultMode())
    })
    
    output$barplotOutput <- renderUI({
      min_height <- 400
      height <- max(min_height, ncol(eselist@read_reports[[getReportType()]]) * 20)
      barplotOutput(ns("barplot"), height)
    })
    
    output$plotTitle <- renderUI({
      h3(paste(prettifyVariablename(getReportType()), 'plot'))
    })
    
    output$tableTitle <- renderUI({
      h4(paste(prettifyVariablename(getReportType()), 'data'))
    })
    
    
    getReportType <- reactive({
        validate(need(input$reportType, FALSE))
        input$reportType
    })
    
    getDefaultMode <- reactive({
        if (getReportType() == 'read_attrition'){
          'overlay' 
        }else{
          'stack' 
        }
    })
    
    getPlotmatrix <- reactive({
        t(getReportTable())
    })
    
    getReportTable <- reactive({
        plotmatrix <- eselist@read_reports[[getReportType()]]
        plotmatrix <- plotmatrix[,apply(plotmatrix, 2, function(x) sum(x > 10) > 0)]
        plotmatrix <- plotmatrix[,order(colMeans(plotmatrix), decreasing = TRUE)]
        colnames(plotmatrix) <- prettifyVariablename(colnames(plotmatrix))
        plotmatrix
    })
    
    callModule(barplot, "barplot", getPlotmatrix = getPlotmatrix, getYLabel = reactive({
        "Reads"
    }))
    callModule(simpletable, "readrep", displayMatrix = getReportTable, filename = "read_report", rownames = TRUE)
    
} 
