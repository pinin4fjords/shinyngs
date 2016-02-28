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
    
    tagList(uiOutput(ns("geneSets")), radioButtons(ns("overlapType"), "Overlap type", c("union", "intersect")))
    
}

#' The server function of the gene set module
#' 
#' The gene set module is for adding a gene set filter to shiny UI elements.
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' This function assumes that the gene sets have one gene ID (e.g. Entrez)
#' which need to be converted to another (e.g. Symbol, Ensembl gene ID).
#' This would be common when dealign with MSigDB gene sets, for example.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param annotation Dataframe containing gene annotation
#' @param entrezgenefield The column of annotation containing Entrez gene IDs
#' @param genefield The gene ID type in annotation by which results are keyed
#' @param geneset_files A named list of .gmt gene set files as might be 
#' derived from MSigDB
#'
#' @return output A list of two reactive functions: getPathwayNames() and 
#' getPathwayGenes() which will be used by other modules. 
#'
#' @keywords shiny
#' 
#' @examples
#' geneset_functions <- callModule(geneset, 'heatmap', annotation, entrezgenefield, genefield, geneset_files)

geneset <- function(input, output, session, annotation, entrezgenefield, genefield, geneset_files) {
    
    # Generate the UI part
    
    output$geneSets <- renderUI({
        
        gene_sets <- getGeneSets()
        gene_set_names <- structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), 
            names = unlist(lapply(names(gene_sets), function(settype) paste0(prettifyVariablename(names(gene_sets[[settype]])), " (", settype, 
                ")"))))
        
        ns <- session$ns
        selectizeInput(ns("geneSets"), "Gene sets:", choices = gene_set_names, multiple = TRUE, options = list(placeholder = "select one or more gene sets"))
    })
    
    getGeneSets <- reactive({
        
        withProgress(message = "reading gene set info", value = 0, {
            
            derived_gene_sets <- file.path("calculated", "gene_sets.rds")
            
            if (file.exists(derived_gene_sets)) {
                withProgress(message = "retrieving previously loaded sets", value = 0, {
                  gene_sets <- readRDS(derived_gene_sets)
                })
            } else {
                gene_sets <- lapply(geneset_files, GSEABase::getGmt)
                
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
    
    list(getPathwayNames = reactive({
        gene_sets <- getGeneSets()
        lapply(input$geneSets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            names(gene_sets[[pathparts[1]]])[pathparts[2]]
        })
    }), getPathwayGenes = reactive({
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
