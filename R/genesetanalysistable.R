#' The UI input function of the genesetanalysistable module
#' 
#' This module displays gene set analysis tables stored as a list in the 
#' \code{gene_set_analyses} slot of an \code{ExploratorySummarizedExperiment}.
#' The keys of this list must match those of the \code{gene_sets} slot of the
#' containing \code{ExploratorySummarizedExperimentList}, and the row names
#' of each table must match the second-level keys. The module is based on the
#' output of roast() from \code{limma}, but it's fairly generic, and assumes
#' only the presence of a 'p value' and 'FDR' column, so the output of other
#' methods should be easily adapted to suit.
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
#' genesetanalysistableInput('experiment', eselist)

genesetanalysistableInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.
    
    for (exp in names(eselist)) {
        assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    
    field_sets = list(gene_set_types = list(uiOutput(ns("geneSets"))), differential_gene_sets = list(numericInput(ns("pval"), "Maximum p value", value = 0.05), numericInput(ns("fdr"), "Maximum FDR", 
        value = 0.1)), contrasts = contrastsInput(ns("genesetanalysistable"), default_max_p = 0.05, default_max_q = 1, default_min_foldchange = 1.2))
    
    # Things we don't want to wrap in a field set - probably hidden stuff
    
    naked_fields = list()
    
    if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
        field_sets$select_assay_data <- expression_filters
    } else {
        naked_fields <- pushToList(naked_fields, expression_filters)
    }
    
    field_sets <- c(field_sets, list(export = simpletableInput(ns("genesetanalysistable"), "Gene set analysis")))
    
    list(naked_fields, fieldSets(ns("fieldset"), field_sets))
}

#' The output function of the genesetanalysistable module
#' 
#' This module provides information on the comparison betwen pairs of groups 
#' defined in a 'tests' slot of the ExploratorySummarizedExperiment
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
#' genesetanalysistableOutput('experiment')

genesetanalysistableOutput <- function(id) {
    ns <- NS(id)
    
    list(modalInput(ns("genesetanalysistable"), "help", "help"), modalOutput(ns("genesetanalysistable"), "Gene set analysis", includeMarkdown(system.file("inlinehelp", "genesetanalysistable.md", 
        package = packageName()))), simpletableOutput(ns("genesetanalysistable"), tabletitle = "Gene set analysis"))
}

#' The server function of the genesetanalysistable module
#' 
#' This module displays gene set analysis tables stored as a list in the 
#' \code{gene_set_analyses} slot of an \code{ExploratorySummarizedExperiment}.
#' The keys of this list must match those of the \code{gene_sets} slot of the
#' containing \code{ExploratorySummarizedExperimentList}, and the row names
#' of each table must match the second-level keys. The module is based on the
#' output of roast() from \code{limma}, but it's fairly generic, and assumes
#' only the presence of a 'p value' and 'FDR' column, so the output of other
#' methods should be easily adapted to suit.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example). Essentially this just passes the results of \code{colData()} 
#' applied to the specified SummarizedExperiment object to the 
#' \code{simpletable} module
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
#' callModule(genesetanalysistable, 'genesetanalysistable', eselist)

genesetanalysistable <- function(input, output, session, eselist) {
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    # For each experiment with gene set analysis, only keep assays associated with gene set results, so that the assay select doesn't have invalid options.
    
    for (exp in names(eselist)) {
        assays(eselist[[exp]]) <- assays(eselist[[exp]])[names(eselist[[exp]]@gene_set_analyses)]
    }
    
    # Extract the gene sets that have been analysed for the the user to select from
    
    ns <- session$ns
    
    output$geneSets <- renderUI({
        genesetselectInput(ns("genesetanalysistable"))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, select_assays = TRUE, select_samples = FALSE, select_genes = FALSE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "genesetanalysistable", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = FALSE))
    
    # Parse the gene sets for ease of use
    
    unpack.list(callModule(genesetselect, "genesetanalysistable", eselist, getExperiment, filter_by_type = TRUE, require_select = FALSE))
    
    observe({
        updateGeneSetsList()
    })
    
    getGeneSetAnalysis <- reactive({
        validate(need(input$pval, "Waiting for p value"), need(input$fdr, "Waiting for FDR value"))
        
        ese <- getExperiment()
        assay <- getAssay()
        gene_set_types <- getGeneSetTypes()
        selected_contrasts <- getSelectedContrasts()
        
        gst <- ese@gene_set_analyses[[assay]][[gene_set_types]][[selected_contrasts]]
        
        # Rename p value if we have PValue from mroast etc()
        
        colnames(gst) <- sub("PValue", "p value", colnames(gst))
        
        # Select out specific gene sets if they've been provided
        
        selected_gene_sets <- getGenesetNames()
        if (!is.null(selected_gene_sets)) {
            validate(need(any(selected_gene_sets %in% rownames(gst)), "Selected gene sets not available in test results"))
            gst <- gst[selected_gene_sets, , drop = FALSE]
        }
        
        # Move the row names to an actual column
        
        gst <- data.frame(gst, check.names = FALSE, stringsAsFactors = FALSE)
        gst$gene_set_id <- rownames(gst)
        gst <- gst[, c("gene_set_id", colnames(gst)[colnames(gst) != "gene_set_id"]), drop = FALSE]
        
        # Apply the user's filters
        
        gst <- gst[gst[["p value"]] < input$pval & gst[["FDR"]] < input$fdr, , drop = FALSE]
        
        validate(need(nrow(gst) > 0, "No results matching specified filters"))
        
        if (nrow(gst) > 0) {
            
            # Add in the differential genes
            
            ct <- filteredContrastsTables()[[1]]
            up <- convertIds(rownames(ct)[ct[["Fold change"]] >= 0], ese, ese@labelfield)
            down <- convertIds(rownames(ct)[ct[["Fold change"]] < 0], ese, ese@labelfield)
            
            gene_sets <- getGeneSets()
            
            gst$significant_genes <- apply(gst, 1, function(row) {
                if (row["Direction"] == "Up") {
                  siggenes <- intersect(gene_sets[[getGeneSetTypes()]][[row["gene_set_id"]]], up)
                } else {
                  siggenes <- intersect(gene_sets[[getGeneSetTypes()]][[row["gene_set_id"]]], down)
                }
                paste(siggenes, collapse = " ")
            })
            
            gst
        }
    })
    
    # Take the table and add links etc
    
    getDisplayGeneSetAnalysis <- reactive({
        gst <- getGeneSetAnalysis()
        
        # Add links, but use a prettiefied version of the gene set name that re-flows to take up less space
        
        gst <- linkMatrix(gst, eselist@url_roots, data.frame(gene_set_id = prettifyGeneSetName(gst$gene_set_id)))
        colnames(gst) <- prettifyVariablename(colnames(gst))
        
        gst
    })
    
    # Make an explantory file name
    
    makeFileName <- reactive({
        gsub("[^a-zA-Z0-9_]", "_", paste("gsa", getSelectedContrastNames(), getGeneSetTypes()))
    })
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "genesetanalysistable", downloadMatrix = getGeneSetAnalysis, displayMatrix = getDisplayGeneSetAnalysis, filename = makeFileName, rownames = FALSE)
}
