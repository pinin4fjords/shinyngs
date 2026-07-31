#' The input function of the chipseq module
#'
#' This provides the form elements to control the ChIP-seq display. The chipseq
#' module combines output from analysis modules such as PCA and boxplots into a
#' comprehensive application.
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
#' chipseqInput("chipseq", eselist)
#'
chipseqInput <- function(id, eselist) {
  explorerAppInput(id, eselist, "chipseq")
}

#' The server function of the chipseq module
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
#' chipseq("chipseq", eselist)
#'
chipseq <- function(id, eselist, heatmap_layout = heatmap_layout_options()) {
  explorerAppServer(id, eselist, "chipseq", heatmap_layout)
}
