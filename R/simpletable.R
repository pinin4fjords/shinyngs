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
#' simpletableInput('mytable', 'this is a table')

simpletableInput <- function(id, description = NULL) {
    ns <- NS(id)
    
    inputs <- list()
    if (!is.null(description)) {
        pushToList(inputs, p(description))
    }
    
    inputs <- pushToList(inputs, downloadButton(ns("downloadTable"), "Download table"))
    
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
        outputs <- pushToList(outputs, h4(tabletitle))
    }
    
    outputs <- pushToList(outputs, DT::dataTableOutput(ns("datatable")))
    
    tagList(outputs)
}

#' The server function of the simpletable module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param datatable Data frame with a table of data
#' @param pageLength Number of items per page
#' @param filename A string to use in the name of the table download
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(simpletable, 'simpletable', my_data_frame)

simpletable <- function(input, output, session, getDatatable, pageLength = 25, filename='datatable') {
  
    output$datatable = DT::renderDataTable({
        getDatatable()
    }, options = list(pageLength = pageLength, escape = F), rownames = FALSE)
    
    output$downloadTable <- downloadHandler(filename = paste0(filename, ".csv"), content = function(file) {
        write.csv(getDatatable(), file = file)
    })
    
} 
