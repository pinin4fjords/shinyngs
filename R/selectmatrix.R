#' The UI input function of the selectmarix module
#'  
#' This module uses the geneselect and sampleselect modules to parse controls
#' defining matrix, row and column and return a matrix.
#' 
#' This will generally not be called directly, but by other modules such as the
#' heatmap module.
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' selectmatrixInput(ns('heatmap'), se, group_vars, default_groupvar)

selectmatrixInput <- function(id, ses) {
    
    ns <- NS(id)
    
    inputs <- list(selectInput(ns("experiment"), "Experiment", names(ses)), uiOutput(ns("assay")))
    
    # Replace experiment with a hidden input if we've got just the one
    
    if (length(ses) == 1) {
        inputs[[1]] <- hiddenInput(ns("experiment"), names(ses)[1])
    }
    
    return(tagList(inputs))
}

#' The server function of the selectmatrix module
#' 
#' This module uses the geneselect and sampleselect modules to parse controls
#' defining matrix, row and column and return a subsetted matrix.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param ses List of StructuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#' @param var_n The number of rows to select when doing so by variance. Default = 50
#' @param var_max The maximum umber of rows to select when doing so by variance. 
#' Default = 500
#'
#' @return output A list of reactive functions for fetching the derived matrix 
#' and making a title based on its properties.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', se)

selectmatrix <- function(input, output, session, ses, var_n = 50, var_max = 500, select_genes = TRUE) {
    
    output$assay <- renderUI({
        
        validate(need(!is.null(input$experiment), "Waiting for form to provide experiment"))
        
        ns <- session$ns
        
        se <- ses[[input$experiment]]
        
        if (length(GenomicRanges::assays(se)) > 1) {
            assayselect <- selectInput(ns("assay"), "Matrix", names(GenomicRanges::assays(se)))
        } else {
            assayselect <- hiddenInput(ns("assay"), names(GenomicRanges::assays(se))[1])
        }
        list(assayselect, sampleselectInput(ns("selectmatrix"), ses[[input$experiment]]), geneselectInput(ns("selectmatrix"), select_genes = select_genes))
    })
    
    # Reactive for getting the right SummarizedExperiment and passing it on to sample and gene selection
    
    getExperiment <- reactive({
        ses[[input$experiment]]
    })
    
    getAssay <- reactive({
        input$assay
    })
    
    # Use the sampleselect and geneselect modules to generate reactive expressions that can be used to derive an expression matrix
    
    selectSamples <- callModule(sampleselect, "selectmatrix", getExperiment)
    
    geneselect_functions <- callModule(geneselect, "selectmatrix", getExperiment, var_n = var_n, var_max = var_max, selectSamples = selectSamples, assay = getAssay)
    selectRows <- geneselect_functions$selectRows
    
    # Generate an expression matrix given the selected experiment, assay, rows and columns
    
    selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            validate(need(!is.null(input$assay), "Waiting for form to provide assay"), need(length(selectSamples()) > 0, "Waiting for sample selection"))
            GenomicRanges::assays(getExperiment())[[getAssay()]][selectRows(), selectSamples(), drop = FALSE]
        })
    })
    
    # Extract experimental variables given selection parameters
    
    selectColData = reactive({
        validate(need(length(selectSamples()) > 0, "Waiting for sample selection"))
        data.frame(colData(getExperiment())[selectSamples(), ])
    })
    
    # Return the list of reactive expressions we'll need to access the data
    
    list(getExperiment = getExperiment, selectMatrix = selectMatrix, title = geneselect_functions$title, selectColData = selectColData)
} 
