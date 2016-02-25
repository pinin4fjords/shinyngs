genesetInput <- function(id, gene_set_dir, gene_set_type_names) {
    ns <- NS(id)
    
    tagList(uiOutput(ns("geneSets")), radioButtons(ns("overlapType"), "Overlap type", 
        c("union", "intersect")))
    
}

geneset <- function(input, output, session, annotation, entrezgenefield, genefield, 
    gene_set_dir, gene_set_type_names) {
    
    # Generate the UI part
    
    output$geneSets <- renderUI({
        
        gene_sets <- getGeneSets()
        gene_set_names <- structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 
            1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(names(gene_sets), 
            function(settype) paste0(prettifyVariablename(names(gene_sets[[settype]])), 
                " (", gsub("_", " ", gene_set_type_names[[settype]]), ")"))))
        
        ns <- session$ns
        selectizeInput(ns("geneSets"), "Gene sets:", choices = gene_set_names, multiple = TRUE, 
            options = list(placeholder = "select one or more gene sets"))
    })
    
    getGeneSets <- reactive({
        
        withProgress(message = "reading gene set info", value = 0, {
            
            derived_gene_sets <- file.path("calculated", "gene_sets.rds")
            
            if (file.exists(derived_gene_sets)) {
                withProgress(message = "retrieving previously loaded sets", value = 0, 
                  {
                    gene_sets <- readRDS(derived_gene_sets)
                  })
            } else {
                require(GSEABase)
                gene_set_files <- list.files(gene_set_dir, full.names = TRUE, pattern = ".gmt")
                gene_sets <- lapply(gene_set_files, getGmt)
                names(gene_sets) <- unlist(lapply(gene_set_files, function(x) sub(".entrez.gmt", 
                  "", basename(x))))
                
                gene_sets <- sapply(gene_sets, function(x) structure(sapply(x, function(y) {
                  set_gene_ids <- as.integer(geneIds(y))
                  structure(annotation[match(set_gene_ids, annotation[[entrezgenefield]]), 
                    genefield], names = set_gene_ids)
                }), names = names(x)), simplify = FALSE, USE.NAMES = TRUE)
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
