#' The input function of the illuminaarray module
#'
#' This provides the form elements to control the Illumina expression array
#' display. The illuminaarray module combines output from analysis modules such
#' as PCA and boxplots into a comprehensive application.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' illuminaarrayInput("illuminaarray", eselist)
#'
illuminaarrayInput <- function(id, eselist) {
  explorerAppInput(id, eselist, "illuminaarray")
}

#' The server function of the illuminaarray module
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param heatmap_layout A list of heatmap pixel-size options as produced by
#'   \code{\link{heatmap_layout_options}}, passed through to every heatmap
#'   panel in the app
#'
#' @keywords shiny
#'
#' @examples
#' illuminaarray("illuminaarray", eselist)
#'
illuminaarray <- function(id, eselist, heatmap_layout = heatmap_layout_options()) {
  explorerAppServer(id, eselist, "illuminaarray", heatmap_layout)
}
