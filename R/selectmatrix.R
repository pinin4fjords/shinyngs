#' The UI input function of the selectmarix module
#' 
#' This module forms the core of many operations in \code{shinyngs}. To use the
#' matrix data stored in an \code{ExploratorySummarizedExperimentList} object, 
#' selection must be made on the basis of experiment (which 
#' \code{ExploratorySummarizedExperiment}) to use), the specific assay of the 
#' experiment, and the rows and columns of the selected assay matrix. This 
#' module provides customisable controls for selection at each of these levels, 
#' and parses those inputs to produce matrices used in the various plots. 
#' 
#' The \code{\link{geneselect}} and \code{\link{sampleselect}} modules provide
#' row- and column- selection, respectively.

#' This will generally not be called directly, but by other modules such as the 
#' heatmap module.
#' 
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param require_tests Only use elements of \code{eselist} that have a 
#'   populated \code{tests} slot. For plots using p value data etc, this is 
#'   used to hide experiments that don't have the necessary data.
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
    
    # Restrict to valid experiments
    
    if (require_tests) {
        eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
            length(ese@tests) > 0
        })))]
    }
    inputs <- list(selectInput(ns("experiment"), "Experiment", names(eselist)), uiOutput(ns("assay")), uiOutput(ns("samples")), uiOutput(ns("rows")), uiOutput(ns("meta")))
    
    # Replace experiment with a hidden input if we've got just the one
    
    if (length(eselist) == 1) {
        inputs[[1]] <- hiddenInput(ns("experiment"), names(eselist)[1])
    }
    
    return(inputs)
}

#' The server function of the selectmatrix module
#' 
#' This module forms the core of many operations in \code{shinyngs}. To use the
#' matrix data stored in an \code{ExploratorySummarizedExperimentList} object, 
#' selection must be made on the basis of experiment (which 
#' \code{ExploratorySummarizedExperiment}) to use), the specific assay of the 
#' experiment, and the rows and columns of the selected assay matrix. This 
#' module provides customisable controls for selection at each of these levels, 
#' and parses those inputs to produce matrices used in the various plots. 
#' 
#' The \code{\link{geneselect}} and \code{\link{sampleselect}} modules provide
#' row- and column- selection, respectively.

#' This will generally not be called directly, but by other modules such as the 
#' heatmap module.
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
#' @param select_assays Provide UI and functions for assay selection?
#' @param select_samples Provide UI and functions for sample selection?
#'   (Default: TRUE)
#' @param select_genes Provide UI and functions for gene (row) selection? 
#'   (Default: TRUE)
#' @param provide_all_genes Allow the 'all rows' selection in the UI? Means we 
#'   don't have to calculate variance so the display is quicker, but it's a bad 
#'   idea for e.g. heatmaps where the visual scales by the numbre of rows.
#' @param default_gene_select The default method to use for selecting genes
#' @param require_tests Only use elements of \code{eselist} that have a 
#'   populated \code{tests} slot. For plots using p value data etc, this is 
#'   used to hide experiments that don't have the necessary data.
#' @param rounding Number of decimal places to show in results (Default 2)
#'   
#' @return output A list of reactive functions for fetching the derived matrix 
#'   and making a title based on its properties.
#'   
#' @keywords shiny
#'   
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', eselist)

selectmatrix <- function(input, output, session, eselist, var_n = 50, var_max = NULL, select_assays = TRUE, select_samples = TRUE, select_genes = TRUE, provide_all_genes = FALSE, 
    default_gene_select = NULL, require_tests = FALSE, rounding = 2, select_meta = TRUE) {
    
    # Use the sampleselect and geneselect modules to generate reactive expressions that can be used to derive an expression matrix
    
    unpack.list(callModule(sampleselect, "selectmatrix", eselist = eselist, getExperiment))
    unpack.list(callModule(geneselect, "selectmatrix", eselist = eselist, getExperiment, var_n = var_n, var_max = varMax(), selectSamples = selectSamples, getAssay = getAssay, 
        provide_all = provide_all_genes, default = default_gene_select))
    
    # Render controls for selecting the experiment (where a user has supplied multiple SummarizedExpression objects in a list) and assay within each
    
    ns <- session$ns
    
    output$assay <- renderUI({
        withProgress(message = "Rendering assay drop-down", value = 0, {
            ns <- session$ns
            
            if (length(validAssays()) > 1 && select_assays) {
                assayselect <- selectInput(ns("assay"), "Matrix", validAssays())
            } else {
                assayselect <- hiddenInput(ns("assay"), validAssays()[1])
            }
            
            assayselect
        })
    })
    
    # Alow users to add extra metadata columns to the display
    
    output$meta <- renderUI({
      ese <- getExperiment()
      if (select_meta){
        metafields <- colnames(mcols(ese))
        if (length(ese@idfield) > 0){
          metafields <- setdiff(metafields, ese@idfield)
        }
        
        checkboxGroupInput(ns("metafields"), "Add meta fields", structure(metafields, names = prettifyVariablename(metafields)), selected = ese@labelfield, inline = TRUE)
      }else if (length(ese@labelfield) > 0){
        hiddenInput(id = ns("metafields"), values = ese@labelfield) 
      }
    })
    
    getMetafields <- reactive({
        input$metafields
    })
    
    # Render sample selection controls
    
    output$samples <- renderUI({
        sampleselectInput(ns("selectmatrix"), eselist = eselist, getExperiment = getExperiment, select_samples = select_samples)
    })
    
    # Render row selection controls
    
    output$rows <- renderUI({
        geneselectInput(ns("selectmatrix"), select_genes = select_genes)
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
        eid <- getExperimentId()
        eselist[[eid]]
    })
    
    # Name of the experment is useful sometimes
    
    getExperimentId <- reactive({
        validate(need(input$experiment, "Waiting for experiment selection"))
        input$experiment
    })
    
    getExperimentName <- reactive({
        eid <- getExperimentId()
        prettifyVariablename(eid)
    })
    
    # Get the row labels where available
    
    getRowLabels <- reactive({
        withProgress(message = "Deriving row labels", value = 0, {
            ese <- getExperiment()
            # if (!is.null(ese@idfield)) {
            idToLabel(rownames(ese), ese)
            # } else { rownames(ese) }
        })
    })
    
    # Allow calling modules to retrieve the current assay
    
    getAssay <- reactive({
        validate(need(!is.null(input$assay), "Waiting for form to provide assay"))
        input$assay
    })
    
    # Retrieve the assay measure to display with plots etc (where defined by the user)
    
    getAssayMeasure <- reactive({
        ese <- getExperiment()
        if (length(ese@assay_measures) > 0 && getAssay() %in% names(ese@assay_measures)) {
            ese@assay_measures[[getAssay()]]
        } else {
            "expression"
        }
    })
    
    varMax <- reactive({
        if (is.null(var_max)) {
            nrow(getExperiment())
        } else {
            var_max
        }
    })
    
    getAssayMatrix <- reactive({
      ese <- getExperiment()
      assay <- getAssay()
      
      SummarizedExperiment::assays(ese)[[assay]]
    })
    
    # Generate an expression matrix given the selected experiment, assay, rows and columns
    
    selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            validate(need(!is.null(input$assay), "Waiting for form to provide assay"), need(length(selectSamples()) > 0, "Waiting for sample selection"), need(length(selectRows()) > 
                0, "No matching rows in selected matrix"))
            
            assay_matrix <- getAssayMatrix()
            selected_matrix <- assay_matrix[selectRows(), selectSamples(), drop = FALSE]
            if (getSampleSelect() == "group" && getSummaryType() != "none") {
                selected_matrix <- summarizeMatrix(selected_matrix, data.frame(selectColData())[[getSampleGroupVar()]], getSummaryType())
            }
            
            # This just to deal with annoying dimension-dropping beviour of apply() on a single-row matrix
            
            if (nrow(selected_matrix) == 1) {
                selected_matrix[1, ] <- apply(selected_matrix, 2, round, rounding)
                selected_matrix
            } else {
                apply(selected_matrix, 2, round, rounding)
            }
        })
    })
    
    # Extract experimental variables given selection parameters
    
    selectColData = reactive({
        validate(need(length(selectSamples()) > 0, "Waiting for sample selection"))
        withProgress(message = "Extracting experiment metadata", value = 0, {
            droplevels(data.frame(colData(getExperiment())[selectSamples(), , drop = FALSE]))
        })
    })
    
    # Calling modules may need to know if the data are sumamrised. E.g. heatmaps only need to display sample metadata for unsummarised matrices Will only be
    # summarised if grouping variables were supplied!
    
    isSummarised <- reactive({
        length(eselist@group_vars) > 0 && getSummaryType() != "none"
    })
    
    # Extract the annotation from the SummarizedExperiment
    
    getAnnotation = reactive({
        ese <- getExperiment()
        validate(need(ncol(mcols(ese)) > 0, "Selected experiment contains no row metadata"))
        withProgress(message = "Deriving annotation", value = 0, {
            data.frame(mcols(ese))
        })
    })
    
    # Use selectMatrix() to get the data matrix, then apply the appropriate labels. Useful in cases where the matrix is destined for display
    
    selectLabelledMatrix <- reactive({
        
        selected_matrix <- data.frame(selectMatrix(), check.names = FALSE)
        se <- getExperiment()
        
        labelMatrix(selected_matrix, se, metafields = getMetafields())
    })
    
    # Use selectLabelledMatrix to get the labelled matrix and add some links.
    
    selectLabelledLinkedMatrix <- reactive({
        selected_matrix <- selectLabelledMatrix()
        se <- getExperiment()
        
        if (length(eselist@url_roots) > 0) {
            linkMatrix(selected_matrix, eselist@url_roots)
        } else {
            selected_matrix
        }
    })
    
    # Accessors for the id and label fields of the current experiment
    
    getIdField <- reactive({
        ese <- getExperiment()
        if (length(ese@idfield) > 0) {
            ese@idfield
        } else {
            "id"
        }
    })
    
    getLabelField <- reactive({
        ese <- getExperiment()
        if (length(ese@labelfield) > 0) {
            ese@labelfield
        } else {
            # ese@idfield
            NULL
        }
    })
    
    # getAssayIds <- reactive({
    #   assay_matrix <- getAssayMatrix()
    #   
    #   rownames(assay_matrix[complete.cases(assay_matrix),,drop=F])
    # })
    
    # Return the list of reactive expressions we'll need to access the data
    
    list(getExperiment = getExperiment, getAssayMeasure = getAssayMeasure, selectMatrix = selectMatrix, selectLabelledMatrix = selectLabelledMatrix, matrixTitle = title, 
        selectColData = selectColData, isSummarised = isSummarised, getAssay = getAssay, selectLabelledLinkedMatrix = selectLabelledLinkedMatrix, getRowLabels = getRowLabels, 
        getAnnotation = getAnnotation, getIdField = getIdField, getLabelField = getLabelField, getExperimentId = getExperimentId, getExperimentName = getExperimentName, getNonEmptyRows = getNonEmptyRows, getMetafields = getMetafields)
}

#' Add columns to display ID and label in a table
#'
#' Labels only added if \code{labelfield} is specified in \code{ese}
#'
#' @param matrix The input table
#' @param ese An ExploratorySummarizedExperiment object
#'
#' @return output Table with columns added

labelMatrix <- function(matrix, ese, idcol = NULL, metafields = c()) {
  
    idfield <- "id"
    if (length(ese@idfield) > 0) {
        idfield <- ese@idfield
    }

    # If we're just using the row names as IDs
        
    if (is.null(idcol)) {
        datacolnames <- colnames(matrix)
        matrix[[idfield]] <- rownames(matrix)
    } else {
        datacolnames <- colnames(matrix)[colnames(matrix) != idcol]
        colnames(matrix)[colnames(matrix) == idcol] <- idfield
    }
    
    # Add in the meta fields if specified
    
    for (mf in metafields){
      matrix[[mf]] <- convertIds(matrix[[idfield]], ese, mf)
    }
    
    matrix <- matrix[,c(idfield, metafields, datacolnames), drop = FALSE]
    colnames(matrix)[match(metafields, colnames(matrix))] <- prettifyVariablename(metafields)
    colnames(matrix)[colnames(matrix) == idfield] <- prettifyVariablename(idfield)
    
    #if (length(ese@labelfield) > 0) {
    #    labelfield <- ese@labelfield
    #    matrix[[labelfield]] <- convertIds(matrix[[idfield]], ese, labelfield)
    #    matrix <- matrix[, c(idfield, labelfield, datacolnames), drop = FALSE]
        
    #    colnames(matrix)[colnames(matrix) == labelfield] <- prettifyVariablename(labelfield)
    #} else {
    #    matrix <- matrix[, c(idfield, datacolnames), drop = FALSE]
    #}
    
    matrix
}

#' Add links to a table
#'
#' Root URLs must be present in the \code{url_roots} slot of \code{se}
#'
#' @param matrix The input table
#' @param url_roots A list with URL roots, with names matching columns of 
#' \code{matrix}
#' @param display_values A matrix which may contain values for displaying 
#' in the link, differerent from that used in the href.
#'
#' @return output Table with links added

linkMatrix <- function(matrix, url_roots, display_values = data.frame()) {
  
    withProgress(message = "Adding links", value = 0, {

        # Add prettified version of each field in URL roots in case matrix column names are prettified
        
        for (fieldname in names(url_roots)) {
            url_roots[[prettifyVariablename(fieldname)]] <- url_roots[[fieldname]]
        }
        
        for (fieldname in names(url_roots)) {
            if (fieldname %in% colnames(matrix)) {
              
                notna <- ! is.na(matrix[[fieldname]])
                fvs_for_href <- fvs_for_display <- matrix[[fieldname]][notna]
                if (fieldname %in% colnames(display_values)) {
                  fvs_for_display <- display_values[[fieldname]][notna]
                }
                
                # Use a simple column paste for single-value columns. Different
                # aproach for multi-value columns
                
                if (any(grepl(' ', matrix[[fieldname]]))){
                  fvs_for_href <- strsplit(fvs_for_href, ' ')
                  fvs_for_display <- strsplit(fvs_for_display, ' ')
                  
                  matrix[[fieldname]][notna] <- unlist(lapply(1:length(fvs_for_href), function(x){
                    paste(paste0("<a href='", url_roots[fieldname], fvs_for_href[[x]], "'>", fvs_for_display[[x]], "</a>"), collapse = ' ')
                  }))
                }else{
                  matrix[[fieldname]][notna] <- paste0("<a href='", url_roots[fieldname], fvs_for_href, "'>", fvs_for_display, "</a>")
                }
            }
        }
        matrix
    })
}

#' Create row labels based on the settings of \code{labelfield} in the 
#' \code{ExploratorySummarizedExperiment} object and the annotation data in 
#' \code{mcols}.
#'
#' @param list of ids
#' @param ese An ExploratorySummarizedExperiment
#' @param sep Separator for ID and label fields
#'
#' @return String vector of same length as \code{ids}
#' @export

idToLabel <- function(ids, ese, sep = " / ") {
    if (length(ese@labelfield) == 0) {
        ids
    } else {
        labels <- convertIds(ids, ese, ese@labelfield)
        labels[!is.na(labels)] <- paste(labels[!is.na(labels)], ids[!is.na(labels)], sep = sep)
        labels[is.na(labels)] <- ids[is.na(labels)]
        labels
    }
}

#' Convert row names to metadata identifiers
#'
#' @param ids IDs found as row names in the 
#' \code{ExploratorySummarizedExperiment}
#' @param ese The \code{ExploratorySummarizedExperiment}
#' @param to The metadata column (via \code{mcols}) to use
#' @param remove_na Take out NAs? Not done by default to preserve vector length
#'
#' @return output Vector of converted ids
#' @export

convertIds <- function(ids, ese, to, remove_na = FALSE) {
    annotation <- data.frame(mcols(ese))
    
    multi <- grepl(" ", ids)
    
    # converted <- annotation[match(ids, annotation[[ese@idfield]]), to]
    converted <- annotation[match(ids, rownames(ese)), to]
    
    # If some elements contained multiple values try splitting them
    
    multi_ids <- lapply(ids[multi], function(x) unlist(strsplit(x, " ")))
    # converted[multi] <- unlist(lapply(multi_ids, function(x) paste(annotation[match(x, annotation[[ese@idfield]]), to], collapse = ' ')))
    converted[multi] <- unlist(lapply(multi_ids, function(x) paste(annotation[match(x, rownames(ese)), to], collapse = " ")))
    
    if (remove_na) {
        converted <- converted[!is.na(converted)]
    }
    converted
}

#' Is there only one matrix to plot from this object?
#' 
#' Convenience function for deciding how to construct filters
#'
#' @param eselist 
#'
#' @return output Logical value
#' @export

singleValidMatrix <- function(eselist) {
    length(eselist) == 1 && length(assays(eselist[[1]])) == 1
}
