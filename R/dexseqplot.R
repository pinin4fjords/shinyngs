#' The UI input function of the dexseqplot Shiny module
#' 
#' This module produces a differential exon usage plot using the \code{plotDEXSeq} 
#' function of the DEXSeq package. 
#' 
#' For the plot to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}. 
#' 
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the 
#' \code{ExploratorySummarizedExperiment}.
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
#' dexseqplotInput('experiment', eselist)

dexseqplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Re-use field from dexseqtable
    
    table_fields <- dexseqtableInputFields(ns("deuPlotTable"), eselist, allow_filtering = FALSE)
    
    field_sets <- list(gene = list(labelselectfieldInput(ns("genesymbol"))), differential_exon_usage = c(table_fields$differential_exon_usage, 
        list(numericInput(ns("deuQvalPlotMax"), "Maximum false discovery rate", value = 0.1), checkboxInput(ns("deuExpression"), "Show expression plot?", 
            value = TRUE), checkboxInput(ns("deuSplicing"), "Show exon usage (adjusted for gene expression)", value = TRUE), checkboxInput(ns("deuNorcounts"), 
            "Show normalised counts plot?", value = FALSE), checkboxInput(ns("deuDisplayTranscripts"), "Show transcripts?", value = TRUE))), 
        export = list(p(plotdownloadInput(ns("deuPlot"), "DEU plot")), br(), p(table_fields$export)))
    
    fieldSets(ns("fieldset"), field_sets)
}

#' The UI output function of the dexseqplot Shiny module. Produces a plot and a 
#' table of values.
#' 
#' This module produces a differential exon usage plot using the \code{plotDEXSeq} 
#' function of the DEXSeq package. 
#' 
#' For the plot to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}. 
#' 
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the 
#' \code{ExploratorySummarizedExperiment}.
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
#' dexseqplotOutput('experiment', eselist)

dexseqplotOutput <- function(id, eselist) {
    
    ns <- NS(id)
    
    list(modalInput(ns("dexseqplot"), "help", "help"), modalOutput(ns("dexseqplot"), "Differential exon usage plot", includeMarkdown(system.file("inlinehelp", 
        "dexseqplot.md", package = packageName()))), h4("Gene-wise differential exon usage"), plotOutput(ns("deuPlot"), height = 620), dexseqtableOutput(ns("deuPlotTable")))
}

#' The server function of the dexseqplot Shiny module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example). 
#' 
#' This module produces a differential exon usage plot using the \code{plotDEXSeq} 
#' function of the DEXSeq package. 
#' 
#' For the plot to be displayed, the \code{dexseq_results} slot must be filled
#' on at least one of the component \code{ExploratorySummarizedExperiment} objects
#' of the input \code{ExploratorySummarizedExperimentList}. 
#' 
#' \code{dexseq_results} must be a list of \code{DEXSeqResults} objects corresponding
#' to the contrasts listed in the \code{contrasts} slot of the 
#' \code{ExploratorySummarizedExperiment}.
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @keywords shiny
#'   
#' @examples
#' callModule(dexseqplot, 'dexseqplot', eselist)

dexseqplot <- function(input, output, session, eselist) {
    
    # Fetch the table of values for the gene
    
    unpack.list(callModule(dexseqtable, "deuPlotTable", eselist = eselist, allow_filtering = FALSE, getDEUGeneID = getDEUGeneID, show_controls = FALSE, 
        page_length = 50, link_to_deu_plot = FALSE))
    
    # Create a field for selecting the gene ID
    
    unpack.list(callModule(labelselectfield, "genesymbol", eselist = eselist, getExperiment = getExperiment, labels_from_all_experiments = TRUE, 
        url_field = "deu_gene"))
    
    # Get the a gene ID for the currently input gene symbol. Could be a composite
    
    getDEUGeneID <- reactive({
        ese <- getExperiment()
        gene_id <- getSelectedIds()
        deu <- ese@dexseq_results
        selected_contrast_number <- getSelectedContrastNumbers()[[1]][[1]]
        
        d <- deu[[as.numeric(selected_contrast_number)]]
        
        # Sometimes an exon can be part of multiple Ensembl gene records. The group ID will then be a composite like
        # 'ENSRNOG00000051235+ENSRNOG00000051158+ENSRNOG00000000419'
        
        if (!gene_id %in% rownames(d)) {
            gene_id <- unique(grep(gene_id, deu[[1]]$groupID, value = TRUE))[1]
        }
        
        gene_id
    })
    
    # Display the DEU plot
    
    output$deuPlot <- renderPlot({
        
        ese <- getExperiment()
        
        selected_contrast_number <- getSelectedContrastNumbers()[[1]][[1]]
        dexseq_result <- ese@dexseq_results[[as.numeric(selected_contrast_number)]]
        
        selected_contrast <- getSelectedContrasts()[[1]][[1]]
        gene_label <- getSelectedLabels()
        gene_id <- getDEUGeneID()
        
        withProgress(message = "Making differential exon usage plot", value = 0, {
            
            DEXSeq::plotDEXSeq(dexseq_result, geneID = gene_id, FDR = input$deuQvalPlotMax, fitExpToVar = selected_contrast[[1]][1], norCounts = input$deuNorcounts, 
                splicing = input$deuSplicing, displayTranscripts = input$deuDisplayTranscripts, expression = input$deuExpression, names = TRUE, 
                legend = TRUE, cex.axis = 1.2, cex = 1.3, lwd = 2)
        })
    }, height = 600)
    
    # Provide the plot for download
    
    makeDEUPlotForDownload <- reactive({
        
        ese <- getExperiment()
        
        selected_contrast_number <- getSelectedContrastNumbers()[[1]][[1]]
        dexseq_result <- ese@dexseq_results[[selected_contrast_number]]
        selected_contrast <- getSelectedContrasts()[[1]][[1]]
        gene_label <- getSelectedLabels()
        gene_id <- getDEUGeneID()
        
        DEXSeq::plotDEXSeq(dexseq_result, geneID = gene_id, FDR = input$deuQvalPlotMax, fitExpToVar = selected_contrast[[1]][1], norCounts = input$deuNorcounts, 
            splicing = input$deuSplicing, displayTranscripts = input$deuDisplayTranscripts, expression = input$deuExpression, names = TRUE, 
            legend = TRUE, cex.axis = 1.2, cex = 1.3, lwd = 2)
    })
    
    # Call to plotdownload module
    
    callModule(plotdownload, "deuPlot", makePlot = makeDEUPlotForDownload, filename = "deuplot.png", plotHeight = 800, plotWidth = 1200)
    
    # Return the reactive for updating the gene input field. Will be used for updating the field when linking to this panel
    
    updateLabelField
    
} 
