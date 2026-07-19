#' The input function of the summarizematrix module
#'
#' This module provides a form element and associated get function for defining
#' how a summary statistic is calculated (probably by mean).
#'
#' @param id Submodule namespace
#' @param allow_none Allow a 'no summarisation' selection.
#' @param select_summary_type Allow user to select summary type (e.g. mean)?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' contrastsInput("test")
#'
summarisematrixInput <- function(id, allow_none = TRUE, select_summary_type = TRUE) {
  ns <- NS(id)

  summaryoptions <- c()
  if (allow_none) {
    summaryoptions <- c(None = "none")
  }

  field <- inline_field(selectInput(ns("summaryType"), NULL, c(summaryoptions, Mean = "colMeans", `Geometric mean` = "col_geom_means", Median = "col_medians"),
    selected = "none"
  ), label = "Average type")

  if (!select_summary_type) {
    field <- shinyjs::hidden(field)
  }

  field
}

#' The server function of the summarisematrix module
#'
#' This module provides a form element and associated get function for defining
#' how a summary statistic is calculated (probably by mean).
#'
#' @param id Module namespace
#'
#' @keywords shiny
#'
#' @examples
#' summarisematrix()
#'
summarisematrix <- function(id) {
  moduleServer(id, function(input, output, session) {
    getSummaryType <- reactive({
      input$summaryType
    })
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
#' summarize_matrix(mymatrix, myfactor)
#'
summarize_matrix <- function(matrix, treatment_factor, summaryFunc = "colMeans") {
  # We need a factor

  if (!is.factor(treatment_factor)) {
    treatment_factor <- factor(treatment_factor)
  }

  # Deal with missing values

  treatment_factor <- na_replace(treatment_factor)

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
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
#' col_geom_means(m)
#'
col_geom_means <- function(x) {
  x <- as.matrix(x)
  positive <- !is.na(x) & x > 0
  logs <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  logs[positive] <- log(x[positive])
  stats::setNames(exp(colSums(logs) / nrow(x)), colnames(x))
}

#' Medians by matrix column
#'
#' @param x A matrix
#'
#' @return Vector with column medians
#'
#' @export
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(NULL, c("a", "b", "c")))
#' col_medians(m)
#'
col_medians <- function(x) {
  x <- as.matrix(x)
  stats::setNames(matrixStats::colMedians(x), colnames(x))
}

#' Identify matrix rows that contain more than one distinct value
#'
#' Missing values are treated as a distinct value, matching the semantics of
#' \code{length(unique(x)) > 1} applied row-wise, so a row of a single non-NA
#' value alongside any \code{NA} counts as having multiple values.
#'
#' @param matrix Numeric matrix
#'
#' @return Logical vector, one element per row, \code{TRUE} where the row holds
#'   more than one distinct value
#'
#' @noRd
rowsWithMultipleValues <- function(matrix) {
  matrix <- as.matrix(matrix)
  n_nan <- rowSums(is.nan(matrix))
  n_missing <- rowSums(is.na(matrix))
  n_na <- n_missing - n_nan
  has_value <- n_missing < ncol(matrix)
  row_min <- matrixStats::rowMins(matrix)
  row_max <- matrixStats::rowMaxs(matrix)
  finite_differs <- has_value & n_missing == 0L & (row_min != row_max)
  (has_value & n_missing > 0L) | finite_differs | (!has_value & n_na > 0L & n_nan > 0L)
}
