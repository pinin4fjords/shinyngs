#' The input function for the \code{modal} module
#'
#' This module uses Shiny's \code{modalDialog()} to create overlaid text for the
#' current panel which displays when a link is clicked. The link is placed in
#' the output function of the calling module, and the modal is shown by
#' \code{modalServer()} in the calling module's server function (see example).
#'
#' This is handy, for example when adding help text.
#'
#' @param id Modal ID. Must match that passed to \code{modalServer}
#' @param label A label to use for the link
#' @param class A class to apply to the link
#' @param icon Icon used to activate modal
#'
#' @examples
#' modalInput(ns("dendro"), "help")
#'
modalInput <- function(id, label, class, icon = "info-circle") {
  ns <- NS(id)

  if (!is.null(icon)) {
    label <- HTML(paste(icon(icon, verify_fa = FALSE), label))
  }

  actionLink(ns("link"), label = label, class = class)
}

#' The server function of the \code{modal} module
#'
#' This module uses Shiny's \code{modalDialog()} to create overlaid text for the
#' current panel which displays when the link produced by \code{modalInput()} is
#' clicked. It is called from the server function of the calling module, using
#' the same id passed to \code{modalInput} (see example).
#'
#' This is handy, for example when adding help text.
#'
#' @param id Modal ID. Must match that passed to \code{modalInput}
#' @param title Title to show on the help modal. May be a function, which is
#'   called each time the modal opens, so titles that depend on reactive state
#'   stay current.
#' @param content Content to include in the modal. May be a function, called
#'   each time the modal opens. When \code{NULL} (the default), Markdown is
#'   loaded once from \code{inst/inlinehelp/<id>.md}.
#'
#' @examples
#' modalServer("dendro", "Sample clustering dendrogram")
#'
modalServer <- function(id, title, content = NULL) {
  moduleServer(id, function(input, output, session) {
    default_body <- if (is.null(content)) {
      includeMarkdown(system.file("inlinehelp", paste0(id, ".md"), package = packageName()))
    }
    observeEvent(input$link, {
      body <- if (is.null(content)) default_body else if (is.function(content)) content() else content
      dialog_title <- if (is.function(title)) title() else title
      showModal(modalDialog(body, title = dialog_title, size = "l", easyClose = TRUE, footer = modalButton("Close")))
    })
  })
}
