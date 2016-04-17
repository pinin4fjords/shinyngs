#' The UI function of the genesetselect module
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
#' genesetselectInput(ns('heatmap'))

genesetselectInput <- function(id, multiple = TRUE) {
    ns <- NS(id)
    
    tagList(selectizeInput(ns("geneSets"), "Gene sets", choices = NULL, options = list(placeholder = "Type a gene set keyword", maxItems = 5), multiple = multiple), 
        radioButtons(ns("overlapType"), "Overlap type", c("union", "intersect")))
}

#' The server function of the genesetselect module
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
#' geneset_functions <- callModule(genesetselect, 'heatmap', getExperiment())

genesetselect <- function(input, output, session, eselist, getExperiment, multiple = TRUE) {
    
    # Get a list of names to show for the gene sets
    
    getGeneSetNames <- reactive({
        gene_sets <- getGeneSets()
        
        structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(names(gene_sets), 
            function(settype) paste0(prettifyGeneSetName(names(gene_sets[[settype]])), " (", settype, ")"))))
    })
    
    # A reactive for relating codes back to gene set IDs
    
    getGeneSetCodesByIDs <- reactive({
        gene_sets <- getGeneSets()
        structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(names(gene_sets), 
            function(settype) names(gene_sets[[settype]]))))
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list. This reactive must be called by
    # the calling module.
    
    updateGeneSetsList <- reactive({
        updateSelectizeInput(session, "geneSets", choices = getGeneSetNames(), server = TRUE)
    })
    
    # Get gene sets with the proper label field keying
    
    getGeneSets <- reactive({
        ese <- getExperiment()
        eselist@gene_sets[[ese@labelfield]]
    })
    
    # Rerieve and validate the gene set selection
    
    getInputGeneSets <- reactive({
        validate(need(input$geneSets, "Please select a gene set"))
        
        if ((!multiple)) {
            validate(need(length(input$geneSets) == 1, "Please select a single gene set only"))
        }
        
        input$geneSets
    })
    
    # Return list of reactive expressions
    
    list(getGeneSets = getGeneSets, updateGeneSetsList = updateGeneSetsList, getPathwayNames = reactive({
        
        gene_sets <- getGeneSets()
        input_gene_sets <- getInputGeneSets()
        
        unlist(lapply(input_gene_sets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            names(gene_sets[[pathparts[1]]])[pathparts[2]]
        }))
    }), getGenesetTypes = reactive({
        
        gene_sets <- getGeneSets()
        input_gene_sets <- getInputGeneSets()
        
        unique(unlist(lapply(input_gene_sets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            names(gene_sets)[pathparts[1]]
        })))
    }), getPathwayGenes = reactive({
        
        gene_sets <- getGeneSets()
        input_gene_sets <- getInputGeneSets()
        
        gene_sets <- getGeneSets()
        path_gene_sets <- lapply(input_gene_sets, function(pathcode) {
            pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
            gene_sets[[pathparts[1]]][[pathparts[2]]]
        })
        
        if (input$overlapType == "union") {
            
            # Use c to preserve names
            
            Reduce(c, path_gene_sets)
        } else {
            
            # Again- this is more than a simple Reduce(intersect because of the need to preserve names
            
            path_gene_sets[[1]][Reduce(intersect, lapply(path_gene_sets, names))]
        }
    }), updateGeneset = reactive({
        query <- parseQueryString(session$clientData$url_search)
        geneset_codes <- getGeneSetCodesByIDs()
        validate(need(query$geneset %in% names(geneset_codes), "Invalid gene set ID entered"))
        
        geneset_code <- getGeneSetCodesByIDs()[query$geneset]
        updateSelectizeInput(session, "geneSets", selected = geneset_code, choices = getGeneSetNames(), server = TRUE)
    }))
    
}

#' Prettify gene set names like those from MSigDB
#'
#' @param gsn Gene set name like 'KEGG_GLYCOLOYSIS_GLUCONEOGENESIS'
#'
#' @return output Prettified version
#' @export

prettifyGeneSetName <- function(gsn) {
    saveRDS(gsn, file = "~/shinytests/gsn.rds")
    words <- strsplit(gsn, "_")
    
    unlist(lapply(words, function(w) paste(w[1], paste(tolower(w[-1]), collapse = " "))))
} 
