#' The UI input function of the selectmarix module
#' 
#' This module uses the geneselect and sampleselect modules to parse controls 
#' defining matrix, row and column and return a matrix.
#' 
#' This will generally not be called directly, but by other modules such as the 
#' heatmap module.
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
#' selectmatrixInput(ns('heatmap'), eselist)

selectmatrixInput <- function(id, eselist, require_tests = FALSE) {
    
    ns <- NS(id)
    
    if (require_tests) {
        eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
            length(ese@tests) > 0
        })))]
    }
    inputs <- list(selectInput(ns("experiment"), "Experiment", names(eselist)), uiOutput(ns("assay")))
    
    # Replace experiment with a hidden input if we've got just the one
    
    if (length(eselist) == 1) {
        inputs[[1]] <- hiddenInput(ns("experiment"), names(eselist)[1])
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
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param var_n The number of rows to select when doing so by variance. Default
#'   = 50
#' @param var_max The maximum umber of rows to select when doing so by variance.
#'   Default = 500
#' @param select_samples Provide UI and functions for sample selection?
#'   (Default: TRUE)
#' @param select_genes Provide UI and functions for gene (row) selection? 
#'   (Default: TRUE)
#' @param provide_all_genes Allow the 'all rows' selection in the UI? Means we 
#'   don't have to calculate variance so the display is quicker, but it's a bad 
#'   idea for e.g. heatmaps where the visual scales by the numbre of rows.
#' @param rounding Number of decimal places to show in results (Default 2)
#'   
#' @return output A list of reactive functions for fetching the derived matrix 
#'   and making a title based on its properties.
#'   
#' @keywords shiny
#'   
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', eselist)

selectmatrix <- function(input, output, session, eselist, var_n = 50, var_max = NULL, select_samples = TRUE, select_genes = TRUE, provide_all_genes = FALSE, require_tests = FALSE, 
    rounding = 2) {
    
    # Use the sampleselect and geneselect modules to generate reactive expressions that can be used to derive an expression matrix
    
    unpack.list(callModule(sampleselect, "selectmatrix", eselist = eselist, getExperiment))
    unpack.list(callModule(geneselect, "selectmatrix", eselist = eselist, getExperiment, var_n = var_n, var_max = varMax(), selectSamples = selectSamples, assay = getAssay, 
        provide_all = provide_all_genes))
    
    # Render controls for selecting the experiment (where a user has supplied multiple SummarizedExpression objects in a list) and assay within each
    
    output$assay <- renderUI({
        withProgress(message = "Rendering assay drop-down", value = 0, {
            validate(need(!is.null(input$experiment), "Waiting for form to provide experiment"))
            
            ns <- session$ns
            
            ese <- getExperiment()
            
            if (length(validAssays()) > 1) {
                assayselect <- selectInput(ns("assay"), "Matrix", validAssays())
            } else {
                assayselect <- hiddenInput(ns("assay"), validAssays()[1])
            }
            list(assayselect, sampleselectInput(ns("selectmatrix"), eselist = eselist, getExperiment = getExperiment, select_samples = select_samples), geneselectInput(ns("selectmatrix"), 
                select_genes = select_genes))
            
        })
    })
    
    # Get list of assays
    
    validAssays <- reactive({
        ese <- getExperiment()
        
        if (require_tests) {
            valid_assays <- names(ese@tests)
        } else {
            names(SummarizedExperiment::assays(ese))
        }
    })
    
    # Reactive for getting the right ExploratorySummarizedExperiment and passing it on to sample and gene selection
    
    getExperiment <- reactive({
        validate(need(input$experiment, FALSE))
        eselist[[input$experiment]]
    })
    
    # Get the row labels where available
    
    getRowLabels <- reactive({
        withProgress(message = "Deriving row labels", value = 0, {
            ese <- getExperiment()
            if (!is.null(ese@idfield)) {
                idToLabel(rownames(ese), ese)
            } else {
                rownames(ese)
            }
        })
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
    
    # Generate an expression matrix given the selected experiment, assay, rows and columns
    
    selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            validate(need(!is.null(input$assay), "Waiting for form to provide assay"), need(length(selectSamples()) > 0, "Waiting for sample selection"))
            selected_matrix <- SummarizedExperiment::assays(getExperiment())[[getAssay()]][selectRows(), selectSamples(), drop = FALSE]
            selected_matrix <- selected_matrix[complete.cases(selected_matrix), ]
            
            if (getSampleSelect() == "group" && getSummaryType() != "none") {
                selected_matrix <- summarizeMatrix(selected_matrix, data.frame(selectColData())[[getSampleGroupVar()]], getSummaryType())
            }
            apply(selected_matrix, 2, round, rounding)
        })
    })
    
    # Extract experimental variables given selection parameters
    
    selectColData = reactive({
        validate(need(length(selectSamples()) > 0, "Waiting for sample selection"))
        withProgress(message = "Extracting experiment metadata", value = 0, {
            droplevels(data.frame(colData(getExperiment())[selectSamples(), , drop = FALSE]))
        })
    })
    
    # Calling modules may need to know if the data are sumamrised. E.g. heatmaps only need to display sample metadata for unsummarised matrices Will only be summarised if
    # grouping variables were supplied!
    
    isSummarised <- reactive({
        length(eselist@group_vars) > 0 && getSummaryType() != "none"
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
        
        if (length(eselist@url_roots) > 0) {
            linkMatrix(selected_matrix, se, eselist@url_roots)
        } else {
            selected_matrix
        }
    })
    
    # Return the list of reactive expressions we'll need to access the data
    
    list(getExperiment = getExperiment, selectMatrix = selectMatrix, selectLabelledMatrix = selectLabelledMatrix, matrixTitle = title, selectColData = selectColData, isSummarised = isSummarised, 
        getAssay = getAssay, selectLabelledLinkedMatrix = selectLabelledLinkedMatrix, getRowLabels = getRowLabels)
}

#' Add columns to display ID and label in a table
#'
#' Labels only added if \code{labelfield} is specified in \code{se}
#'
#' @param matrix The input table
#' @param ese An ExploratorySummarizedExperiment object
#'
#' @return output Table with columns added

labelMatrix <- function(matrix, ese) {
    
    withProgress(message = "Adding labels to matrix", value = 0, {
        
        datacolnames <- colnames(matrix)
        
        idfield <- ese@idfield
        matrix[[idfield]] <- rownames(matrix)
        
        if (length(ese@labelfield) > 0) {
            annotation <- data.frame(mcols(ese))
            labelfield <- ese@labelfield
            
            matrix[[labelfield]] <- annotation[match(rownames(matrix), annotation[[idfield]]), labelfield]
            matrix <- matrix[, c(idfield, labelfield, datacolnames)]
            
            colnames(matrix)[colnames(matrix) == labelfield] <- prettifyVariablename(labelfield)
        } else {
            matrix <- matrix[, c(idfield, datacolnames)]
        }
        
        # Make the field identifiers nicer
        
        colnames(matrix)[colnames(matrix) == idfield] <- prettifyVariablename(idfield)
    })
    matrix
}

#' Add links to a table
#'
#' Root URLs must be present in the \code{url_roots} slot of \code{se}
#'
#' @param matrix The input table
#' @param ese An ExploratorySummarizedExperiment object
#'
#' @return output Table with links added

linkMatrix <- function(matrix, ese, url_roots) {
    
    withProgress(message = "Adding links to matrix", value = 0, {
        
        for (fieldtype in c("idfield", "labelfield")) {
            
            fieldname <- slot(ese, fieldtype)
            
            if (length(fieldname) > 0) {
                
                if (fieldname %in% names(url_roots)) {
                  
                  # Field name was prettified in selectLabelledMatrix(), so we have to use the prettified version to access the column
                  
                  p_fieldname <- prettifyVariablename(fieldname)
                  matrix[[p_fieldname]] <- paste0("<a href='", url_roots[fieldname], matrix[[p_fieldname]], "'>", matrix[[p_fieldname]], "</a>")
                }
            }
        }
        
    })
    matrix
}

#' Create row labels based on the settings of \code{labelfield} in the 
#' SummarizedExperiment object and the annotation data in \code{mcols}
#'
#' @param list of ids
#' @param ese An ExploratorySummarizedExperiment
#'
#' @return String vector of same length as \code{ids}
#' @export

idToLabel <- function(ids, ese) {
    if (length(ese@labelfield) == 0) {
        ids
    } else {
        annotation <- data.frame(mcols(ese))
        labels <- annotation[match(ids, annotation[[ese@idfield]]), ese@labelfield]
        labels[!is.na(labels)] <- paste(labels[!is.na(labels)], ids[!is.na(labels)], sep = " / ")
        labels[is.na(labels)] <- ids[is.na(labels)]
        labels
    }
} 
