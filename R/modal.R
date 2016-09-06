#' The input function for the \code{modal} module
#' 
#' This module uses modals from \code{shinyBS} to create overlaid text for the
#' current panel which displays when a link is clicked. Input and output
#' functions are placed in the output function of the calling module (see
#' example).
#' 
#' This is handy, for example when adding help text.
#'
#' @param id Modal ID. Must match that passed to \code{inlinehelpOutput} 
#' @param label A label to use for the link
#' @param class A class to apply to the link
#'
#' @examples
#' modalInput(ns('dendro'), 'help'),

modalInput <- function(id, label, class, icon = 'info-circle') {
    ns <- NS(id)
    
    if (! is.null(icon)){
      label <- HTML(paste(icon(icon), label))
    }
    
    actionLink(ns("link"), label = label, `data-toggle` = "modal", `data-target` = paste0("#", ns(id)), class = class)
}

#' The output function of the \code{modal} module
#' 
#' This module uses modals from \code{shinyBS} to create overlaid text for the
#' current panel which displays when a link is clicked. Input and output
#' functions are placed in the output function of the calling module (see
#' example).
#' 
#' This is handy, for example when adding help text.
#'
#' @param id Modal ID. Must match that passed to \code{inlinehelpInput} 
#' @param prefix Prefix for the markdown file in \code{shinyngs} to display
#' @param title Title to show on the help modal
#'
#' @examples
#' modalOutput(ns('dendro'), 'Sample clustering dendrogram', includeMarkdown(system.file('inlinehelp', 'dendro.Rmd', package = packageName())))

modalOutput <- function(id, title, content) {
    ns <- NS(id)
    shinyBS::bsModal(ns(id), title, ns(id), size = "large", content)
} 
