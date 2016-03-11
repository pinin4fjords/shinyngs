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
#' @param select_samples Provide UI and functions for sample selection? (Default: 
#' TRUE)
#' @param select_genes Provide UI and functions for gene (row) selection? 
#' (Default: TRUE)
#' @param provide_all_genes Allow the 'all rows' selection in the UI? Means we
#' don't have to calculate variance so the display is quicker, but it's a bad
#' idea for e.g. heatmaps where the visual scales by the numbre of rows.
#' @param rounding Number of decimal places to show in results (Default 2)
#'
#' @return output A list of reactive functions for fetching the derived matrix 
#' and making a title based on its properties.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', se)

selectmatrix <- function(input, output, session, ses, var_n = 50, var_max = NULL, select_samples = TRUE, select_genes = TRUE, provide_all_genes = FALSE, rounding = 2) {
    
    # Render controls for selecting the experiment (where a user has supplied multiple SummarizedExpression objects in a list) and assay within each
    
    output$assay <- renderUI({
        
        validate(need(!is.null(input$experiment), "Waiting for form to provide experiment"))
        
        ns <- session$ns
        
        se <- getExperiment()
        
        if (length(GenomicRanges::assays(se)) > 1) {
            assayselect <- selectInput(ns("assay"), "Matrix", names(GenomicRanges::assays(se)))
        } else {
            assayselect <- hiddenInput(ns("assay"), names(GenomicRanges::assays(se))[1])
        }
        list(assayselect, sampleselectInput(ns("selectmatrix"), getExperiment(), select_samples = select_samples), geneselectInput(ns("selectmatrix"), select_genes = select_genes))
    })
    
    
    # Reactive for getting the right SummarizedExperiment and passing it on to sample and gene selection
    
    getExperiment <- reactive({
        validate(need(input$experiment, FALSE))
        ses[[input$experiment]]
    })
    
    # Allow calling modules to retrieve the current assay
    
    getAssay <- reactive({
        input$assay
    })
    
    varMax <- reactive({
        if (is.null(var_max)) {
            nrow(getExperiment())
        } else {
            var_max
        }
    })
    
    # Use the sampleselect and geneselect modules to generate reactive expressions that can be used to derive an expression matrix
    
    unpack.list(callModule(sampleselect, "selectmatrix", getExperiment))
    unpack.list(callModule(geneselect, "selectmatrix", getExperiment, var_n = var_n, var_max = varMax(), selectSamples = selectSamples, assay = getAssay, provide_all = provide_all_genes))
    
    # Generate an expression matrix given the selected experiment, assay, rows and columns
    
    selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            validate(need(!is.null(input$assay), "Waiting for form to provide assay"), need(length(selectSamples()) > 0, "Waiting for sample selection"))
            selected_matrix <- GenomicRanges::assays(getExperiment())[[getAssay()]][selectRows(), selectSamples(), drop = FALSE]
            
            if (getSampleSelect() == "group" && getSummaryType() != "none") {
                saveRDS(selected_matrix, file = "~/shinytests/selected_matrix.rds")
                selected_matrix <- summarizeMatrix(selected_matrix, data.frame(selectColData())[[getSampleGroupVar()]], getSummaryType())
            }
            
            apply(selected_matrix, 2, round, rounding)
        })
    })
    
    # Extract experimental variables given selection parameters
    
    selectColData = reactive({
        validate(need(length(selectSamples()) > 0, "Waiting for sample selection"))
        droplevels(data.frame(colData(getExperiment())[selectSamples(), ]))
    })
    
    # Calling modules may need to know if the data are sumamrised. E.g. heatmaps only need to display sample metadata for unsummarised matrices
    
    isSummarised <- reactive({
        getSummaryType() != "none"
    })
    
    # Extract the annotation from the SummarizedExperiment
    
    getAnnotation = reactive({
        data.frame(mcols(getExperiment()))
    })
    
    # Use selectMatrix() to get the data matrix, then apply the appropriate labels. Useful in cases where the matrix is destined for display
    
    selectLabelledMatrix <- reactive({
        
        selected_matrix <- data.frame(selectMatrix())
        se <- getExperiment()
        
        labelMatrix(selected_matrix, se)
    })
    
    # Use selectLabelledMatrix to get the labelled matrix and add some links.
    
    selectLabelledLinkedMatrix <- reactive({
        selected_matrix <- selectLabelledMatrix()
        se <- getExperiment()
        
        linkMatrix(selected_matrix, se)
    })
    
    # Return the list of reactive expressions we'll need to access the data
    
    list(getExperiment = getExperiment, selectMatrix = selectMatrix, selectLabelledMatrix = selectLabelledMatrix, matrixTitle = title, selectColData = selectColData, 
        isSummarised = isSummarised, getAssay = getAssay, selectLabelledLinkedMatrix = selectLabelledLinkedMatrix)
}

#' Add columns to display ID and label in a table
#'
#' Labels only added if \code{labelfield} is specified in \code{se}
#'
#' @param matrix The input table
#' @param se A SummarizedExperiment object
#'
#' @return output Table with columns added

labelMatrix <- function(matrix, se) {
    datacolnames <- colnames(matrix)
    
    idfield <- metadata(se)$idfield
    matrix[[idfield]] <- rownames(se)
    
    if ("labelfield" %in% names(metadata(se))) {
        annotation <- data.frame(mcols(se))
        labelfield <- metadata(se)$labelfield
        
        matrix[[labelfield]] <- annotation[match(rownames(matrix), annotation[[idfield]]), labelfield]
        matrix <- matrix[, c(metadata(se)$idfield, metadata(se)$labelfield, datacolnames)]
        
        colnames(matrix)[colnames(matrix) == labelfield] <- prettifyVariablename(labelfield)
    } else {
        matrix <- matrix[, c(metadata(se)$idfield, datacolnames)]
    }
    
    # Make the field identifiers nicer
    
    colnames(matrix)[colnames(matrix) == idfield] <- prettifyVariablename(idfield)
    
    matrix
}

#' Add links to a table
#'
#' Root URLs must be present in the \code{url_roots} slot of \code{se}
#'
#' @param matrix The input table
#' @param se A SummarizedExperiment object
#'
#' @return output Table with links added

linkMatrix <- function(matrix, se) {
    
    idfield <- metadata(se)$idfield
    
    if ("url_roots" %in% names(metadata(se))) {
        url_roots <- metadata(se)$url_roots
        
        if (idfield %in% names(url_roots)) {
            
            # Field name was prettified in selectLabelledMatrix(), so we have to use the prettified version to access the column
            
            p_idfield <- prettifyVariablename(idfield)
            matrix[[p_idfield]] <- paste0("<a href='", url_roots[idfield], matrix[[p_idfield]], "'>", matrix[[p_idfield]], "</a>")
        }
        
    }
    matrix
} 
