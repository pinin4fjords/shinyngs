#' The UI function of the gene set module
#' 
#' The gene set module is for adding a gene set filter to shiny UI elements.
#' This will generally not be called directly, but by other modules
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' genesetInput(ns('heatmap'))

genesetInput <- function(id) {
    ns <- NS(id)
    
    tagList(selectizeInput(ns("geneSets"), "Gene sets", choices = NULL, options = list(placeholder = "Type a gene set keyword", maxItems = 5)), radioButtons(ns("overlapType"), "Overlap type", 
        c("union", "intersect")))
}

#' The server function of the gene set module
#' 
#' The gene set module is for adding a gene set filter to shiny UI elements. 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' This function assumes that the gene sets have one gene ID (e.g. Entrez) which
#' need to be converted to another (e.g. Symbol, Ensembl gene ID). This would be
#' common when dealign with MSigDB gene sets, for example.
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist An ExploratorySummarizedExperimentList with its gene_sets
#' slot set
#' @param getExperiment Accessor for returning an
#'   ExploratorySummarizedExperiment object, with 'entrezgenefield', 
#'   'labelfield' set in its slots
#'
#' @return output A list of two reactive functions: getPathwayNames() and 
#'   getPathwayGenes() which will be used by other modules.
#'   
#' @keywords shiny
#'   
#' @examples
#' geneset_functions <- callModule(geneset, 'heatmap', getExperiment())

geneset <- function(input, output, session, eselist, getExperiment) {
    
    # Get a list of names to show for the gene sets
    
    getGeneSetNames <- reactive({
        gene_sets <- getGeneSets()
        structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(names(gene_sets), function(settype) paste0(prettifyVariablename(names(gene_sets[[settype]]), 
            tolower = TRUE), " (", settype, ")"))))
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list. This reactive must be called by the calling module.
    
    updateGeneSetsList <- reactive({
        updateSelectizeInput(session, "geneSets", choices = getGeneSetNames(), server = TRUE)
    })
    
    # Pull in the gene sets from file. These will be cached as an R object for Quicker retrieval next time the app is run.
    
    getGeneSets <- reactive({
        
        # Derive the necessary information from the experiment object
        
        ese <- getExperiment()
        annotation <- data.frame(mcols(ese))
        entrezgenefield <- ese@entrezgenefield
        genefield <- ese@labelfield
        gene_sets <- eselist@gene_sets
        
        withProgress(message = "processing gene sets", value = 0, {
            
            derived_gene_sets <- file.path("calculated", "gene_sets.rds")
            
            if (file.exists(derived_gene_sets)) {
                withProgress(message = "retrieving previously loaded sets", value = 0, {
                  gene_sets <- readRDS(derived_gene_sets)
                })
            } else {
                
                # Convert gene IDs in the gene sets (but leave them keyed by entrez id)
                
                gene_sets <- sapply(gene_sets, function(x) structure(sapply(x, function(y) {
                  set_gene_ids <- as.integer(GSEABase::geneIds(y))
                  structure(annotation[match(set_gene_ids, annotation[[entrezgenefield]]), genefield], names = set_gene_ids)
                }), names = names(x)), simplify = FALSE, USE.NAMES = TRUE)
                
                # Save output to R object for faster future retrieval
                
                dir.create(dirname(derived_gene_sets), showWarnings = FALSE)
                saveRDS(gene_sets, derived_gene_sets)
            }
        })
        gene_sets
        
    })
    
    # Return list of reactive expressions
    
    list(getGeneSets = getGeneSets, updateGeneSetsList = updateGeneSetsList, getPathwayNames = reactive({
        validate(need(input$geneSets, "Waiting for gene set input for names"))
        gene_sets <- getGeneSets()
        lapply(input$geneSets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            names(gene_sets[[pathparts[1]]])[pathparts[2]]
        })
    }), getPathwayGenes = reactive({
        validate(need(input$geneSets, "Waiting for gene set input for genes"))
        
        gene_sets <- getGeneSets()
        path_gene_sets <- lapply(input$geneSets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            gene_sets[[pathparts[1]]][[pathparts[2]]]
        })
        
        if (input$overlapType == "union") {
            Reduce(union, path_gene_sets)
        } else {
            Reduce(intersect, path_gene_sets)
        }
    }))
    
} 
