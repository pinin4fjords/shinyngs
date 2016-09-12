#' The UI input function of the simpletable module
#'  
#' This module produces a simple datatable output with a download button.
#'
#' @param id Submodule namespace
#' @param tabletitle Table title. Will be used on download button.
#' @param description A string to display in the side bar to explain the table.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' simpletableInput('mytable', 'this is a table')

simpletableInput <- function(id, tabletitle = "", description = NULL) {
    ns <- NS(id)
    
    inputs <- list()
    if (!is.null(description)) {
        inputs <- pushToList(inputs, p(description))
    }
    
    inputs <- pushToList(inputs, downloadButton(ns("downloadTable"), paste(tabletitle, "table")))
    
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
        outputs <- pushToList(outputs, h3(tabletitle))
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
#' @param downloadMatrix Reactive expression for retrieving the plot to supply
#' for download (default: NULL, in which case \code{displayMatrix()} will be 
#' used)
#' @param displayMatrix Reactive expression for retrieving the plot for display
#' @param pageLength Number of items per page
#' @param filename A string to use in the name of the table download
#' @param rownames Passed to \code{\link[DT]{renderDataTable}}. Display the row 
#' names of the input matrix? (default: FALSE)
#' @param show_controls Show search box, controls etc on resulting data table? 
#' (default: TRUE)
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(simpletable, 'simpletable', my_data_frame)

simpletable <- function(input, output, session, downloadMatrix = NULL, displayMatrix, pageLength = 15, filename, rownames = FALSE, show_controls = TRUE) {
    
    if (is.null(downloadMatrix)) {
        downloadMatrix <- displayMatrix
    }
    
    options <- list(pageLength = pageLength, lengthMenu = list(c(5, 10, 15, 25, 50, 100), c("5", "10", "15", "25", "50", "100")))
    
    if (!show_controls) {
        options$dom <- "t"
    }
    
    output$datatable = DT::renderDataTable({
        displayMatrix()
    }, options = options, rownames = rownames, escape = FALSE)
    
    output$downloadTable <- downloadHandler(filename = function() {
        if (is.reactive(filename)) {
            filename <- filename()
        }
        paste0(filename, ".csv")
    }, content = function(file) {
        write.csv(downloadMatrix(), file = file, row.names = rownames)
    })
    
}
