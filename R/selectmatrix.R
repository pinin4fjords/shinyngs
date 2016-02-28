#' The UI input function of the selectmarix module
#'  
#' This module uses the geneselect and sampleselect modules to parse controls
#' defining matrix, row and column and return a matrix.
#' 
#' This will generally not be called directly, but by other modules such as the
#' heatmap module.
#'
#' @param id Submodule namespace
#' @param group_vars The variables from the structured experiment that should
#' be used to control sample grouping in the plot
#' @param The default grouping variable to use
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' selectmatrixInput(ns("heatmap"), se, group_vars, default_groupvar)

selectmatrixInput <- function(id, se, group_vars, default_groupvar) {
    
    ns <- NS(id)
    
    tagList(selectInput(ns("assay"), "Matrix", names(assays(se))), sampleselectInput(ns("selectmatrix"), group_vars, default_groupvar), geneselectInput(ns("selectmatrix")))
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
#' @param se StructuredExperiment object with assay and experimental data
#' @param transcriptfield The main identifier for the rows in the assay data.
#' This could be transcript ID, but also probe etc.
#' @param entrezgenefield The column of annotation containing Entrez gene IDs
#' @param genefield The gene ID type in annotation by which results are keyed
#' @param geneset_files A named list of .gmt gene set files as might be 
#' derived from MSigDB
#'
#' @return output A list of reactive functions for fetching the derived matrix 
#' and making a title based on its properties.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, "selectmatrix", se)

selectmatrix <- function(input, output, session, se, transcriptfield, entrezgenefield, genefield, geneset_files) {
    
    selectSamples <- callModule(sampleselect, "selectmatrix", se)
    
    geneselect_functions <- callModule(geneselect, "selectmatrix", se, transcriptfield, entrezgenefield, genefield, geneset_files, selectSamples = selectSamples)
    selectRows <- geneselect_functions$selectRows
    
    list(selectMatrix = reactive({
        se[selectRows(), selectSamples()]
    }), title = geneselect_functions$title)
} 
