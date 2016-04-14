#' The UI input function of the genesetbarcodeplot module
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
#' genesetbarcodeplotInput('experiment', eselist)

genesetbarcodeplotInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    expression_filters <- selectmatrixInput(ns("expression"), eselist)
    fieldSets(ns("fieldset"), list(gene_set = genesetInput(ns('genesetbarcodeplot')), contrast = contrastsInput(ns("genesetbarcodeplot"), allow_filtering = FALSE), select_assay_data = expression_filters))
}

#' The output function of the genesetbarcodeplot module
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
#' genesetbarcodeplotOutput('experiment')

genesetbarcodeplotOutput <- function(id) {
    ns <- NS(id)
    
    plotOutput(ns("genesetbarcodeplot"))
}

#' The server function of the genesetbarcodeplot module
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
#' callModule(genesetbarcodeplot, 'genesetbarcodeplot', eselist)

genesetbarcodeplot <- function(input, output, session, eselist) {
    
    # Only use experiments with gene set analyses available
    
    eselist <- eselist[unlist(lapply(eselist, function(ese) length(ese@gene_set_analyses) > 0))]
    
    # Render the output area - and provide an input-dependent title
    
    output$genesetbarcodeplot <- renderUI({
        ns <- session$ns
        
        simpletableOutput(ns("genesetbarcodeplot"), tabletitle = paste("Assay data", getAssay(), sep = ": "))
    })
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "expression", eselist, select_samples = FALSE, select_genes = FALSE))
    
    # Pass the matrix to the contrasts module for processing
    
    unpack.list(callModule(contrasts, "genesetbarcodeplot", eselist = eselist, getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = FALSE))
    
    # Parse the gene sets for ease of use
    
    unpack.list(callModule(geneset, "genesetbarcodeplot", eselist, getExperiment))
    
    observe({
      updateGeneSetsList()
    })
    
    # Make the barcode plot using limma
    
    output$genesetbarcodeplot <- renderPlot({
      ct <- filteredContrastsTables()[[1]]
      set_genes <- getPathwayGenes() 
        
        par(cex = 2)
      
        limma::barcodeplot(
          ct$`Fold change`, 
          index = convertIds(rownames(ct), getExperiment(), getExperiment()@entrezgenefield) %in% names(set_genes),
          main = getPathwayNames()
        )
    })
} 
