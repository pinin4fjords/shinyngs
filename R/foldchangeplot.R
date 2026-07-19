foldchangeplot_modal <- list(id = "foldchangeplot", title = "Fold change plots")
foldchangeplot_scatter <- list(scatter_id = "foldchange", filename = "foldchange")

#' The UI input function of the \code{foldchangeplot} module
#'
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
#'
#' Leverages the \code{scatterplot} module
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
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' foldchangeplotInput("myid", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("foldchangeplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
foldchangeplotInput <- function(id, eselist) {
  differentialscatterInput(id, eselist, scatter_id = foldchangeplot_scatter$scatter_id)
}

#' The output function of the \code{foldchangeplot} module
#'
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
#'
#' Leverages the \code{scatterplot} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' foldchangeplotOutput("myid")
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("foldchangeplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
foldchangeplotOutput <- function(id) {
  differentialscatterOutput(id, scatter_id = foldchangeplot_scatter$scatter_id, title = "Fold change plot", modal = foldchangeplot_modal)
}

#' Select which fold change plot threshold lines to draw
#'
#' The fold change filter can apply symmetrically (both up and down) or only
#' in one direction, depending on the cardinality operator and the sign of
#' the limit. This picks the matching subset of rows from the \code{lines}
#' data frame built in \code{\link{buildFoldchangeLines}}, where rows 1-2 are
#' the "no change" diagonal, rows 3-4 the fold-down threshold, and rows 5-6
#' the fold-up threshold.
#'
#' @param lines data.frame of threshold lines with six rows, in the row
#'   order described above
#' @param fccard Fold change cardinality operator, as returned by
#'   \code{getFoldChangeCard()} from the \code{contrasts} module
#' @param fclim Fold change limit, as returned by \code{getFoldChange()}
#'   from the \code{contrasts} module
#'
#' @return A subset of \code{lines}, with unused factor levels dropped
#'
#' @keywords internal
selectFoldchangeLines <- function(lines, fccard, fclim) {
  if (fccard %in% c(">= or <= -", "<= and >= -")) {
    lines
  } else if (fccard == "<=" && sign(fclim) == -1) {
    droplevels(lines[1:4, ])
  } else {
    droplevels(lines[c(1, 2, 5, 6), ])
  }
}

#' The server function of the \code{foldchangeplot} module
#'
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperimentList
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   foldchangeplot("foldchangeplot", eselist)
#'   app <- prepareApp("foldchangeplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
foldchangeplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(foldchangeplot_modal$id, foldchangeplot_modal$title)

    differentialscatterLogic(input, output, session, eselist,
      scatter_id = foldchangeplot_scatter$scatter_id, filename = foldchangeplot_scatter$filename,
      buildTable = buildFoldchangeTable, buildLines = buildFoldchangeLines
    )
  })
}

# Make a table of values to use in the fold change plot. Round the values to save space in the JSON

buildFoldchangeTable <- function(contrast_reactives) {
  withProgress(message = "Compiling fold change plot data", value = 0, {
    sct <- contrast_reactives$selectedContrastsTables()
    ct <- sct[[1]][[1]]
    ct <- round(log2(ct[, 1:2]), 3)

    cont <- contrast_reactives$getSelectedContrasts()[[1]][[1]]
    colnames(ct) <- c(paste0("log2(", cont[2], ")"), paste0("log2(", cont[3], ")"))

    ct
  })
}

# Make a set of dashed lines to overlay on the plot representing thresholds

buildFoldchangeLines <- function(fct, contrast_reactives) {
  fclim <- contrast_reactives$getFoldChange()

  bounds <- finiteAxisRange(fct)

  min <- min(bounds$xmin, bounds$ymin)
  max <- max(bounds$xmax, bounds$ymax)

  lines <- data.frame(
    name = c(rep("No change", 2), rep(paste0(abs(fclim), "-fold down"), 2), rep(paste0(abs(fclim), "-fold up"), 2)), x = c(
      min, max,
      min, max, min, max
    ), y = c(c(min, max), (min - log2(abs(fclim))), (max - log2(abs(fclim))), (min + log2(abs(fclim))), (max + log2(abs(fclim))))
  )
  lines$name <- factor(lines$name, levels = unique(lines$name))

  # Use lines dependent on how the fold change filter is applied

  fccard <- contrast_reactives$getFoldChangeCard()

  selectFoldchangeLines(lines, fccard, fclim)
}
