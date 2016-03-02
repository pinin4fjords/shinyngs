#' The UI input function of the selectmarix module
#'  
#' This module uses the geneselect and sampleselect modules to parse controls
#' defining matrix, row and column and return a matrix.
#' 
#' This will generally not be called directly, but by other modules such as the
#' heatmap module.
#'
#' @param id Submodule namespace
#' @param se StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' selectmatrixInput(ns('heatmap'), se, group_vars, default_groupvar)

selectmatrixInput <- function(id, se) {
    
    ns <- NS(id)
    
    tagList(selectInput(ns("assay"), "Matrix", names(GenomicRanges::assays(se))), sampleselectInput(ns("selectmatrix"), se), geneselectInput(ns("selectmatrix")))
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
#' @param se StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#' @param var_n The number of rows to select when doing so by variance. Default = 50
#' @param var_max The maximum umber of rows to select when doing so by variance. 
#' Default = 500
#'
#' @return output A list of reactive functions for fetching the derived matrix 
#' and making a title based on its properties.
#'
#' @keywords shiny
#' 
#' @examples
#' selectSamples <- callModule(sampleselect, 'selectmatrix', se)

selectmatrix <- function(input, output, session, se, var_n = 50, var_max = 500) {
    
    selectSamples <- callModule(sampleselect, "selectmatrix", se)
    
    geneselect_functions <- callModule(geneselect, "selectmatrix", se, var_n = var_n, var_max = var_max, selectSamples = selectSamples, 
        assay = reactive({
            input$assay
        }))
    selectRows <- geneselect_functions$selectRows
    
    list(selectMatrix = reactive({
        withProgress(message = "Getting expression data subset", value = 0, {
            GenomicRanges::assays(se)[[input$assay]][selectRows(), selectSamples()]
        })
    }), title = geneselect_functions$title, selectColData = reactive({
        data.frame(colData(se)[selectSamples(), ])
    }))
} 
