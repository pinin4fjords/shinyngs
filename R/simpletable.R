#' The UI input function of the simpletable module
#'  
#' This module produces a simple datatable output with a download button.
#'
#' @param id Submodule namespace
#' @param description A string to display in the side bar to explain the table.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' selectmatrixInput(ns('heatmap'), se, group_vars, default_groupvar)

simpletableInput <- function(id, description = NULL) {
    ns <- NS(id)
    
    inputs <- list()
    if (!is.null(description)) {
        inputs[[1]] <- p(description)
    }
    
    inputs[[length(inputs) + 1]] <- downloadButton(ns("downloadTable"), "Download table")
    
    tagList(inputs)
}

#' The output function of the simpletable module
#' 
#' This provides actual datatable element for display by applications
#'
#' @param id Module namespace
#' @param title (optional) Title to display with the table
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' simpletableOutput('simpletable', 'my title')

simpletableOutput <- function(id, tabletitle = NULL) {
    ns <- NS(id)
    
    outputs <- list()
    if (!is.null(tabletitle)) {
        outputs[[1]] <- h4(tabletitle)
    }
    
    outputs[[length(outputs) + 1]] <- DT::dataTableOutput(ns("datatable"))
    
    tagList(outputs)
}

#' The server function of the simpletable module
#' 
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
#' @param datatable Data frame with a table of data
#' @param pageLength Number of items per page
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(simpletable, 'simpletable', my_data_frame)

simpletable <- function(input, output, session, datatable, pageLength = 25) {
    
    output$datatable = DT::renderDataTable({
        datatable
    }, options = list(pageLength = pageLength, escape = F), rownames = FALSE)
    
    output$downloadTable <- downloadHandler(filename = "datatable.csv", content = function(file) {
        write.csv(datatable, file = file)
    })
    
} 
