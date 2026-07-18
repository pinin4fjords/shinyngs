maplot_modal <- list(id = "maplot", title = "MA plots")

#' The UI input function of the \code{maplot} module
#'
#' This module produces an MA plot of log(10) expression vs log(2) fold change
#' for contrasts defined in the `contrasts` slot of an
#' 'ExploratorySummarizedExperimentList` object.
#'
#' Leverages the \code{contrasts} and \code{scatterplot} modules
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
#' maplotInput("myid", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("maplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
maplotInput <- function(id, eselist) {
  differentialScatterInput(id, eselist, scatter_id = "ma")
}

#' The output function of the \code{maplot} module
#'
#' This module produces an MA plot of log(10) expression vs log(2) fold change
#' for contrasts defined in the `contrasts` slot of an
#' 'ExploratorySummarizedExperimentList` object.
#'
#' Leverages the \code{contrasts} and \code{scatterplot} modules
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' maplotOutput("experiment")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("maplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
maplotOutput <- function(id) {
  differentialScatterOutput(id, scatter_id = "ma", title = "MA plot", modal = maplot_modal)
}

#' Select which MA plot threshold lines to draw
#'
#' The fold change filter can apply symmetrically (both up and down) or only
#' in one direction, depending on the cardinality operator and the sign of
#' the limit. This picks the matching subset of rows from the \code{lines}
#' data frame built in \code{\link{buildMaLines}}, where rows 1-2 are the
#' fold-down threshold and rows 3-4 the fold-up threshold.
#'
#' @param lines data.frame of threshold lines with four rows, in the row
#'   order described above
#' @param fccard Fold change cardinality operator, as returned by
#'   \code{getFoldChangeCard()} from the \code{contrasts} module
#' @param fclim Fold change limit, as returned by \code{getFoldChange()}
#'   from the \code{contrasts} module
#'
#' @return A subset of \code{lines}, with unused factor levels dropped
#'
#' @keywords internal
selectMaLines <- function(lines, fccard, fclim) {
  if (fccard %in% c("> or <-", "< and >-")) {
    lines
  } else if (fccard == "<" && sign(fclim) == -1) {
    droplevels(lines[c(1, 2), ])
  } else {
    droplevels(lines[c(3, 4), ])
  }
}

#' The server function of the \code{maplot} module
#'
#' This module is for making scatter plots comparing pairs of groups defined in
#' a 'contrasts' slot of the ExploratorySummarizedExperiment
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
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   maplot("maplot", eselist)
#'   app <- prepareApp("maplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
maplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(maplot_modal$id, maplot_modal$title)

    differentialScatterServer(input, output, session, eselist,
      scatter_id = "ma", filename = "ma",
      buildTable = buildMaTable, buildLines = buildMaLines
    )
  })
}

# Make a table of values to use in the MA plot. Round the values to save space in the JSON

buildMaTable <- function(contrast_reactives) {
  withProgress(message = "Compiling fold change plot data", value = 0, {
    sct <- contrast_reactives$selectedContrastsTables()
    ct <- sct[[1]][[1]]

    matable <- data.frame(round(log10(rowMeans(ct[, 1:2])), 3), round(sign(ct[["Fold change"]]) *
      log2(abs(ct[["Fold change"]])), 3), row.names = rownames(ct), check.names = FALSE)
    colnames(matable) <- c("log(10) mean expression", paste0("log(2) fold change [source scale: ", contrast_reactives$getFoldChangeScale(), "]"))

    matable
  })
}

# Make a set of dashed lines to overlay on the plot representing thresholds

buildMaLines <- function(mat, contrast_reactives) {
  withProgress(message = "Calculating lines", value = 0, {
    fclim <- contrast_reactives$getFoldChange()

    normal_y <- !is.infinite(mat[, 2])
    normal_x <- !is.infinite(mat[, 1])

    ymax <- max(mat[normal_y, 2], na.rm = TRUE)
    ymin <- min(mat[normal_y, 2], na.rm = TRUE)

    xmax <- max(mat[normal_x, 1], na.rm = TRUE)
    xmin <- min(mat[normal_x, 1], na.rm = TRUE)

    lines <- data.frame(
      name = c(rep(paste0(abs(fclim), "-fold down"), 2), rep(paste0(abs(fclim), "-fold up"), 2)), x = c(xmin, xmax, xmin, xmax),
      y = c(rep(-log2(abs(fclim)), 2), rep(log2(abs(fclim)), 2))
    )

    # Use lines dependent on how the fold change filter is applied

    fccard <- contrast_reactives$getFoldChangeCard()

    selectMaLines(lines, fccard, fclim)
  })
}
