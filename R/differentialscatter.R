#' Shared UI input scaffolding for the differential-scatter plot modules
#'
#' \code{\link{volcanoplot}}, \code{\link{maplot}} and \code{\link{foldchangeplot}}
#' are all scatter plots of statistics derived from a contrast in the
#' \code{contrasts} slot of an \code{ExploratorySummarizedExperimentList}, and
#' share the same input scaffolding (expression matrix selection, contrast
#' selection, scatter plot controls, gene highlighting, table export). Only
#' the numeric transform and threshold-line logic genuinely differ between
#' them - see \code{\link{differentialScatterServer}}.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param scatter_id Sub-namespace used for the scatterplot, gene highlighting
#'   and table export controls (e.g. \code{"volcano"}, \code{"ma"},
#'   \code{"foldchange"})
#' @param require_contrast_stats Restrict \code{eselist} to experiments with a
#'   populated \code{contrast_stats} slot before building controls
#' @param multi_view_fn Function of the (possibly filtered) \code{eselist}
#'   returning \code{TRUE} if the expression-matrix controls should be shown
#'   as their own fieldset rather than pushed in as hidden fields
#'
#' @return A list of input tags
#'
#' @keywords shiny
differentialScatterInput <- function(id, eselist, scatter_id, require_contrast_stats = FALSE,
                                      multi_view_fn = function(esel) !singleValidMatrix(esel)) {
  ns <- NS(id)

  # require_contrast_stats filters eselist here, not just in selectmatrixInput below, because
  # multi_view_fn must see the same (possibly narrowed) set of experiments selectmatrixInput will
  # end up offering, not the full unfiltered eselist.
  if (require_contrast_stats) {
    eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
      has_slot_data(ese, "contrast_stats")
    })))]
  }

  expression_filters <- selectmatrixInput(ns("expression"), eselist, require_contrast_stats = require_contrast_stats)
  is_multi_view <- multi_view_fn(eselist)

  fieldsets <- list(contrasts = list(contrastsInput(ns("differential"))))
  if (is_multi_view) {
    fieldsets$expression_matrix <- expression_filters
  }

  fieldsets <- c(fieldsets, list(
    scatter_plot = scatterplotInput(ns(scatter_id)), highlight_points = geneselectInput(ns(scatter_id)),
    export = simpletableInput(ns(paste0(scatter_id, "table")))
  ))

  inputs <- list(fieldSets(ns("fieldset"), fieldsets))

  if (!is_multi_view) {
    inputs <- pushToList(inputs, expression_filters)
  }

  inputs
}

#' Shared output scaffolding for the differential-scatter plot modules
#'
#' @param id Module namespace
#' @param scatter_id Sub-namespace matching the one passed to
#'   \code{\link{differentialScatterInput}}
#' @param title Plot title shown above the output
#' @param modal A list with \code{id} and \code{title} elements identifying
#'   the help modal for this module (e.g. \code{volcanoplot_modal})
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
differentialScatterOutput <- function(id, scatter_id, title, modal) {
  ns <- NS(id)

  moduleMain(
    title,
    scatterplotOutput(ns(scatter_id)),
    htmlOutput(ns(paste0(scatter_id, "table"))),
    help = modalInput(ns(modal$id), "help", "help")
  )
}

#' Shared server logic for the differential-scatter plot modules
#'
#' Wires up the \code{selectmatrix}, \code{contrasts}, \code{geneselect},
#' \code{scatterplot} and \code{simpletable} modules in the way common to
#' \code{\link{volcanoplot}}, \code{\link{maplot}} and
#' \code{\link{foldchangeplot}}. The caller supplies \code{buildTable} to
#' derive the module-specific numeric matrix and \code{buildLines} to derive
#' the module-specific threshold lines; the colorby/label annotation (see
#' \code{\link{annotateDifferentialTable}}) and plot title are identical
#' across all three and applied here.
#'
#' Must be called from within the caller's own \code{moduleServer()} - it does
#' not open its own namespace.
#'
#' @param input,output,session The Shiny input/output/session of the calling
#'   module
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param scatter_id Sub-namespace matching the one passed to
#'   \code{\link{differentialScatterInput}}
#' @param buildTable Function of \code{contrast_reactives} returning the
#'   module-specific data frame, before colorby/label annotation. Its first
#'   two columns are used as the plotted x and y values.
#' @param buildLines Function of the annotated table and
#'   \code{contrast_reactives} returning the threshold-line data frame
#' @param filename Base filename used for the exported table
#' @param require_contrast_stats Restrict \code{eselist} to experiments with a
#'   populated \code{contrast_stats} slot
#'
#' @keywords shiny
differentialScatterServer <- function(input, output, session, eselist, scatter_id, buildTable, buildLines, filename, require_contrast_stats = FALSE) {
  ns <- session$ns

  # Call the selectmatrix module and hold on to the reactives it sends back

  selectmatrix_reactives <- selectmatrix("expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE, require_contrast_stats = require_contrast_stats)

  # Pass the matrix to the contrasts module for processing

  contrast_reactives <- contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE)

  # Call the geneselect module (independently of selectmatrix) to generate sets of genes to highlight

  geneselect_reactives <- geneselect(scatter_id, eselist = eselist, getExperiment = selectmatrix_reactives$getExperiment, getAssay = selectmatrix_reactives$getAssay, provide_all = FALSE, provide_none = TRUE)

  table_id <- paste0(scatter_id, "table")

  output[[table_id]] <- renderUI({
    simpletableOutput(ns(table_id), tabletitle = paste("Plot data for contrast", contrast_reactives$getSelectedContrastNames()[[1]][[1]], sep = ": "), spinner = TRUE)
  })

  # Build the annotated table of values to use in the plot

  scatterTable <- reactive({
    annotateDifferentialTable(buildTable(contrast_reactives), contrast_reactives, geneselect_reactives, selectmatrix_reactives)
  })

  # Make a title by selecting the single contrast name of the single filter set

  getTitle <- reactive({
    contrast_reactives$getSelectedContrastNames()[[1]][[1]]
  })

  # Make a set of dashed lines to overlay on the plot representing thresholds

  plotLines <- reactive({
    buildLines(scatterTable(), contrast_reactives)
  })

  # Extract labels and colors from the annotated table

  scatterLabels <- reactive({
    scatterTable()$label
  })

  getColorby <- reactive({
    scatterTable()$colorby
  })

  # Pass the matrix to the scatterplot module for display

  scatterplot(scatter_id, getDatamatrix = scatterTable, getTitle = getTitle, allow_3d = FALSE, getLabels = scatterLabels, x = 1, y = 2, getColorby = getColorby, getLines = plotLines)

  # Display the data as a table alongside

  simpletable(table_id, downloadMatrix = contrast_reactives$labelledContrastsTable, displayMatrix = contrast_reactives$linkedLabelledContrastsTable, filename = filename, rownames = FALSE, pageLength = 10)
}

#' Annotate a differential-scatter table with colorby and label columns
#'
#' Shared tail of the \code{volcanoTable}/\code{maTable}/\code{foldchangeTable}
#' reactives: marks rows as hidden, matching the contrast filters, or in the
#' highlighted gene set, and attaches display labels for the highlighted rows.
#'
#' @param ct Data frame keyed by feature id, as produced by the
#'   module-specific \code{buildTable} function
#' @param contrast_reactives Reactives returned by the \code{contrasts} module
#' @param geneselect_reactives Reactives returned by the \code{geneselect}
#'   module
#' @param selectmatrix_reactives Reactives returned by the \code{selectmatrix}
#'   module
#'
#' @return The input data frame with \code{colorby} and \code{label} columns
#'   added
#'
#' @keywords shiny
annotateDifferentialTable <- function(ct, contrast_reactives, geneselect_reactives, selectmatrix_reactives) {
  fct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
  ct$colorby <- "hidden"
  ct[rownames(fct), "colorby"] <- "match contrast filters"
  ct[geneselect_reactives$selectRows(), "colorby"] <- "in highlighted gene set"
  ct$colorby <- factor(ct$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))

  ct$label <- idToLabel(rownames(ct), selectmatrix_reactives$getExperiment())
  ct$label[!rownames(ct) %in% c(rownames(fct), geneselect_reactives$selectRows())] <- NA

  ct
}

#' Finite x/y bounds of a differential-scatter table
#'
#' Threshold lines need finite endpoints to span the plotted axis, so
#' infinite values (from log-transforming a zero) are excluded before taking
#' the range.
#'
#' @param table Data frame whose first two columns are the plotted x and y
#'   values
#'
#' @return A list with \code{xmin}, \code{xmax}, \code{ymin} and \code{ymax}
#'
#' @keywords internal
finiteAxisRange <- function(table) {
  normal_x <- !is.infinite(table[, 1])
  normal_y <- !is.infinite(table[, 2])

  list(
    xmin = min(table[normal_x, 1], na.rm = TRUE),
    xmax = max(table[normal_x, 1], na.rm = TRUE),
    ymin = min(table[normal_y, 2], na.rm = TRUE),
    ymax = max(table[normal_y, 2], na.rm = TRUE)
  )
}
