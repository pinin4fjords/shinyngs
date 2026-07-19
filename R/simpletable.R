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
#' simpletableInput("mytable", "this is a table")
#'
simpletableInput <- function(id, tabletitle = "", description = NULL) {
  ns <- NS(id)

  inputs <- list()
  if (!is.null(description)) {
    inputs <- push_to_list(inputs, p(description))
  }

  download_label <- trimws(paste(tabletitle, "table"))
  inputs <- push_to_list(inputs, a11yControl(
    downloadButton(ns("downloadTable"), download_label),
    label = paste("Download", download_label, "as CSV"),
    tooltip = paste("Download the", tolower(download_label), "as a CSV file")
  ))

  tagList(inputs)
}

#' The output function of the simpletable module
#'
#' This provides actual datatable element for display by applications
#'
#' @param id Module namespace
#' @param tabletitle (optional) Title to display with the table
#' @param spinner Show a loading spinner while the table is (re)computed.
#' Intended for tables backed by expensive reactives (e.g. differential
#' expression); left off by default for small, near-instant tables.
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' simpletableOutput("simpletable", "my title")
#'
simpletableOutput <- function(id, tabletitle = NULL, spinner = FALSE) {
  ns <- NS(id)

  outputs <- list()
  if (!is.null(tabletitle)) {
    outputs <- push_to_list(outputs, h3(tabletitle))
  }

  datatable <- DT::dataTableOutput(ns("datatable"))
  if (spinner) {
    datatable <- shinycssloaders::withSpinner(datatable, color = shinyngsSpinnerColor())
  }
  outputs <- push_to_list(outputs, datatable)

  tagList(outputs)
}

#' The server function of the simpletable module
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
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
#' @param filter Passed to \code{DT::renderDataTable()}
#' @param server Passed to \code{DT::renderDataTable()}. Leave at the default
#' (TRUE) for most tables, since it keeps only the current page's data in the
#' browser. Set to FALSE if \code{displayMatrix()} can produce a table with
#' the same dimensions but different column *names* from one render to the
#' next (e.g. column headers derived from a recalculated statistic) - DT's
#' server-side mode matches an in-flight page/sort/search request to fresh
#' data by column name, so a name change between the request and the
#' response raises "the column name ... is not found in data".
#'
#' @keywords shiny
#'
#' @examples
#' simpletable("simpletable", my_data_frame)
#'
simpletable <- function(id, downloadMatrix = NULL, displayMatrix, pageLength = 15, filename, rownames = FALSE, show_controls = TRUE, filter = "none", server = TRUE) {
  moduleServer(id, function(input, output, session) {
    if (is.null(downloadMatrix)) {
      downloadMatrix <- displayMatrix
    }

    options <- list(pageLength = pageLength, lengthMenu = list(c(5, 10, 15, 25, 50, 100), c("5", "10", "15", "25", "50", "100")), dom = "ltip")

    if (show_controls) {
      options$dom <- paste0("f", options$dom)
    }

    output$datatable <- DT::renderDataTable(
      {
        displayMatrix()
      },
      options = options,
      filter = filter,
      rownames = rownames,
      escape = FALSE,
      server = server
    )

    output$downloadTable <- downloadHandler(filename = function() {
      if (is.reactive(filename)) {
        filename <- filename()
      }
      paste0(filename, ".csv")
    }, content = function(file) {
      write.csv(downloadMatrix(), file = file, row.names = rownames)
    })
  })
}
