#' The input function of the summarizematrix module
#' 
#' This module provides a form element and associated get function for defining
#' how a summary statistic is calculated (probably by mean).
#'
#' @param id Submodule namespace
#' @param allow_none Allow a 'no summarisation' selection.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' contrastsInput('test')

summarisematrixInput <- function(id, allow_none = TRUE, select_summary_type = TRUE) {
    
    ns <- NS(id)
    
    summaryoptions <- c()
    if (allow_none) {
        summaryoptions <- c(None = "none")
    }
    
    field <- inlineField(selectInput(ns("summaryType"), NULL, c(summaryoptions, Mean = "colMeans", `Geometric mean` = "colGeomMeans", Median = "colMedians"), 
        selected = "none"), label = "Average type")
    
    if (! select_summary_type){
      field <- shinyjs::hidden(field) 
    }
    
    field
}

#' The server function of the summarisematrix module
#' 
#' This module provides a form element and associated get function for defining
#' how a summary statistic is calculated (probably by mean).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(summarisematrix)

summarisematrix <- function(input, output, session) {
    
    getSummaryType <- reactive({
        input$summaryType
    })
}

#' Summarise the rows of a matrix, applying a function to groups of cells 
#' defined by a factor
#'
#' Note that the specified function will be applied to a tranformed version
#' of the matrix, so \code{colMeans()}, for example, is appropriate.
#'
#' @param matrix Numeric matrix
#' @param treatment_factor a factor defining column groups
#' @param summaryFunc A Function to apply to a transformed version of 
#' \code{matrix} (default: colMeans)
#'
#' @return Summarized matrix, with e.g. means in columns
#'
#' @export
#' 
#' @examples
#' summarizeMatrix(mymatrix, myfactor)

summarizeMatrix <- function(matrix, treatment_factor, summaryFunc = "colMeans") {
    
    # We need a factor
    
    if (!is.factor(treatment_factor)) {
        treatment_factor = factor(treatment_factor)
    }
    
    # Deal with missing values
    
    treatment_factor <- na.replace(treatment_factor)
    
    summaryFunc <- get(summaryFunc)
    t_matrix <- t(matrix)
    
    treatments <- levels(treatment_factor)
    sm <- do.call(cbind, lapply(treatments, function(lev) {
        summaryFunc(t_matrix[treatment_factor == lev, , drop = FALSE])
    }))
    colnames(sm) <- treatments
    sm
}

#' Geometric means by matrix column 
#'
#' @param x A matrix
#'
#' @return Vector with column geometric means
#'
#' @export

colGeomMeans <- function(x) {
    apply(x, 2, geom_mean)
}

#' Geometric mean 
#'
#' @param x A vector
#'
#' @return A numeric value
#'
#' @export

geom_mean <- function(x, na.rm = TRUE) {
    exp(sum(log(x[x > 0]), na.rm = na.rm)/length(x))
}

#' Medians by matrix column 
#'
#' @param x A matrix
#'
#' @return Vector with column medians
#'
#' @export

colMedians <- function(x) {
    apply(x, 2, median)
} 
