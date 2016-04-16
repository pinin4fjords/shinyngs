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
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    fieldSets(ns("fieldset"), list(gene_set_types = uiOutput(ns("geneSetTypes")), differential_gene_sets = list(numericInput(ns("pval"), "Maximum p value", 
        value = 0.05), numericInput(ns("fdr"), "Maximum FDR", value = 0.1)), contrasts = contrastsInput(ns("genesetanalysistable"), default_max_p = 0.05, 
        default_max_q = 1, default_min_foldchange = 1.2), select_assay_data = expression_filters, export = simpletableInput(ns("genesetanalysistable"), "Gene set analysis")))
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
    
    simpletableOutput(ns("genesetanalysistable"), tabletitle = 'Gene set analysis')
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
    
    # Extract the gene sets that have been analysed for the the user to select from
    
    output$geneSetTypes <- renderUI({
        ns <- session$ns
        ese <- getExperiment()
        selectInput(ns("geneSetTypes"), "Gene set type", names(ese@gene_set_analyses), selected = names(ese@gene_set_analyses)[1])
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, select_samples = FALSE, select_genes = FALSE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "genesetanalysistable", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, 
        multiple = FALSE))
    
    # Parse the gene sets for ease of use
    
    unpack.list(callModule(geneset, "genesetanalysistable", eselist, getExperiment))
    
    getGeneSetAnalysis <- reactive({
        validate(need(input$geneSetTypes, "Waiting for gene set type selection"), need(input$pval, "Waiting for p value"), need(input$fdr, "Waiting for FDR value"))
        
        ese <- getExperiment()
        gst <- ese@gene_set_analyses[[input$geneSetTypes]][[getSelectedContrasts()]]
        
        # Rename p value if we have PValue from mroast etc()
        
        colnames(gst) <- sub("PValue", "p value", colnames(gst))
        
        # Move the row naes to an actual column
        
        gst <- data.frame(gst, check.names = FALSE, stringsAsFactors = FALSE)
        gst$gene_set_id <- rownames(gst)
        gst <- gst[, c("gene_set_id", colnames(gst)[colnames(gst) != "gene_set_id"])]
        
        # Apply the user's filters
        
        gst <- gst[gst[["p value"]] < input$pval & gst[["FDR"]] < input$fdr, , drop = FALSE]
        
        if (nrow(gst) > 0) {
            
            # Add in the differential genes
            
            ct <- filteredContrastsTables()[[1]]
            up <- convertIds(rownames(ct)[ct[["Fold change"]] >= 0], ese, ese@labelfield)
            down <- convertIds(rownames(ct)[ct[["Fold change"]] < 0], ese, ese@labelfield)
            
            gene_sets <- getGeneSets()
            
            gst$significant_genes <- apply(gst, 1, function(row) {
                if (row["Direction"] == "Up") {
                  siggenes <- intersect(gene_sets[[input$geneSetTypes]][[row["gene_set_id"]]], up)
                } else {
                  siggenes <- intersect(gene_sets[[input$geneSetTypes]][[row["gene_set_id"]]], down)
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
        gsub("[^a-zA-Z0-9_]", "_", paste("gsa", getSelectedContrastNames(), input$geneSetTypes))
    })
    
    # Pass the matrix to the simpletable module for display
    
    callModule(simpletable, "genesetanalysistable", downloadMatrix = getGeneSetAnalysis, displayMatrix = getDisplayGeneSetAnalysis, filename = makeFileName, 
        rownames = FALSE)
} 
