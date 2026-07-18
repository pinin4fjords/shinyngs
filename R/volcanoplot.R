volcanoplot_modal <- list(id = "volcanoplot", title = "Volcano plots")

#' The UI input function of the \code{volcanoplot} module
#'
#' A volcano plot displays -log(10) of a p value/ FDR against a log(2) fold
#' change on the x axis. This module produces such a plot using the
#' \code{\link{scatterplot}} module (which uses
#' \code{\link[plotly]{plot_ly}})), using data provided by the
#' \code{\link{contrasts}} module based on the setting of \code{contrasts} in
#' \code{eselist}.
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
#' # The volcano module needs an eselist carrying differential statistics
#' # (contrast_stats); see the vignette. It is used via application creation:
#'
#' if (interactive()) {
#'   volcanoplotInput("myid", eselist)
#'   app <- prepareApp("volcanoplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
volcanoplotInput <- function(id, eselist) {
  ns <- NS(id)

  # Only consider experiments that actually have p-values to use in a volcano plot

  eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
    has_slot_data(ese, "contrast_stats")
  })))]
  expression_filters <- selectmatrixInput(ns("expression"), eselist, require_contrast_stats = TRUE)

  # If there's only one experiment with contrast_stats, then the expression filters will just be hidden fields, and there's no point in creating an empty fieldset for
  # them

  fieldsets <- list()
  if (length(eselist) > 1) {
    fieldsets$expression_matrix <- expression_filters
  }

  fieldsets <- c(fieldsets, list(
    contrasts = list(contrastsInput(ns("differential"))), scatter_plot = scatterplotInput(ns("volcano")), highlight_points = geneselectInput(ns("volcano")),
    export = simpletableInput(ns("volcanotable"))
  ))

  inputs <- list(fieldSets(ns("fieldset"), fieldsets))

  if (length(eselist) == 1) {
    inputs <- pushToList(inputs, expression_filters)
  }

  inputs
}

#' The output function of the \code{volcanoplot} module
#'
#' A volcano plot displays -log(10) of a p value/ FDR against a log(2) fold
#' change on the x axis. This module produces such a plot using the
#' \code{\link{scatterplot}} module (which uses
#' \code{\link[plotly]{plot_ly}})), using data provided by the
#' \code{\link{contrasts}} module based on the setting of \code{contrasts} in
#' \code{eselist}.
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
#' differentialtableOutput("experiment")
#'
#' # However, almost certainly called via application creation:
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("volcanoplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
volcanoplotOutput <- function(id) {
  ns <- NS(id)

  moduleMain(
    "Volcano plot",
    scatterplotOutput(ns("volcano")),
    htmlOutput(ns("volcanotable")),
    help = modalInput(ns(volcanoplot_modal$id), "help", "help")
  )
}

#' Select which volcano plot threshold lines to draw
#'
#' The fold change filter can apply symmetrically (both up and down) or only
#' in one direction, depending on the cardinality operator and the sign of
#' the limit. This picks the matching subset of rows from the \code{lines}
#' data frame built in the \code{plotLines} reactive of the
#' \code{volcanoplot} module, where rows 1-2 are the fold-down threshold,
#' rows 3-4 the fold-up threshold, and rows 5-6 the q-value threshold.
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
selectVolcanoLines <- function(lines, fccard, fclim) {
  if (fccard %in% c(">= or <= -", "<= and >= -")) {
    lines
  } else if (fccard == "<=" && sign(fclim) == -1) {
    droplevels(lines[c(1, 2, 5, 6), ])
  } else {
    droplevels(lines[1:4, ])
  }
}

#' The server function of the \code{volcanoplot} module
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example). Essentially this just passes the results of \code{colData()}
#' applied to the specified SummarizedExperiment object to the
#' \code{simpletable} module
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
#' # However, almost certainly called via application creation:
#'
#' if (interactive()) {
#'   differentialtable("differentialtable", eselist)
#'   app <- prepareApp("volcanoplot", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
volcanoplot <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(volcanoplot_modal$id, volcanoplot_modal$title)

    # Call the selectmatrix module and hold on to the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE, require_contrast_stats = TRUE)

    # Pass the matrix to the contrasts module for processing

    contrast_reactives <- contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE)

    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight

    geneselect_reactives <- geneselect("volcano", eselist = eselist, getExperiment = selectmatrix_reactives$getExperiment, getAssay = selectmatrix_reactives$getAssay, provide_all = FALSE, provide_none = TRUE)

    output$volcanotable <- renderUI({
      ns <- session$ns

      simpletableOutput(ns("volcanotable"), tabletitle = paste("Plot data for contrast", contrast_reactives$getSelectedContrastNames()[[1]][[1]], sep = ": "), spinner = TRUE)
    })

    # Pass the matrix to the scatterplot module for display

    scatterplot("volcano", getDatamatrix = volcanoTable, getTitle = getTitle, allow_3d = FALSE, getLabels = volcanoLabels, x = 1, y = 2, getColorby = getColorby, getLines = plotLines)

    # Make a title by selecting the single contrast name of the single filter set

    getTitle <- reactive({
      contrast_names <- contrast_reactives$getSelectedContrastNames()
      contrast_names[[1]][[1]]
    })

    # Make a set of dashed lines to overlay on the plot representing thresholds

    plotLines <- reactive({
      withProgress(message = "Calculating lines", value = 0, {
        vt <- volcanoTable()

        fclim <- contrast_reactives$getFoldChange()
        qvallim <- contrast_reactives$getQval()

        normal_y <- !is.infinite(vt[, 2])
        normal_x <- !is.infinite(vt[, 1])

        ymax <- max(vt[normal_y, 2], na.rm = TRUE)
        ymin <- min(vt[normal_y, 2], na.rm = TRUE)

        xmax <- max(vt[normal_x, 1], na.rm = TRUE)
        xmin <- min(vt[normal_x, 1], na.rm = TRUE)

        lines <- data.frame(
          name = c(
            rep(paste0(abs(fclim), "-fold down"), 2),
            rep(paste0(abs(fclim), "-fold up"), 2),
            rep(paste("q <", qvallim), 2)
          ),
          x = c(rep(-log2(abs(fclim)), 2), rep(log2(abs(fclim)), 2), xmin, xmax),
          y = c(ymin, ymax, ymin, ymax, rep(-log10(qvallim), 2))
        )

        # Use lines dependent on how the fold change filter is applied

        fccard <- contrast_reactives$getFoldChangeCard()

        selectVolcanoLines(lines, fccard, fclim)
      })
    })

    # Extract labels from the volcano table

    volcanoLabels <- reactive({
      withProgress(message = "Making labels", value = 0, {
        vt <- volcanoTable()
        vt$label
      })
    })

    # Extract a vector use to make colors by group

    getColorby <- reactive({
      vt <- volcanoTable()
      vt$colorby
    })

    # Make a table of values to use in the volcano plot. Round the values to save space in the JSON

    volcanoTable <- reactive({
      withProgress(message = "Compiling volcano plot data", value = 0, {
        sct <- contrast_reactives$selectedContrastsTables()
        ct <- sct[[1]][[1]]

        # q values of 0 cause trouble

        ct$`q value`[ct$`q value` == 0] <- min(ct$`q value`[ct$`q value` != 0]) / 10

        ct <- ct[, c("Fold change", "q value")]
        ct[["Fold change"]] <- round(sign(ct[["Fold change"]]) * log2(abs(ct[["Fold change"]])), 3)
        ct[["q value"]] <- round(-log10(ct[["q value"]]), 3)

        cont <- contrast_reactives$getSelectedContrasts()[[1]][[1]]
        fc_axis_label <- paste0("log2(fold change) [source scale: ", contrast_reactives$getFoldChangeScale(), "]")
        colnames(ct) <- c(paste(paste0("(higher in ", cont[2], ")"), fc_axis_label, paste0("(higher in ", cont[3], ")"), sep = "  "), "-log10(q value)")

        fct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
        ct$colorby <- "hidden"
        ct[rownames(fct), "colorby"] <- "match contrast filters"
        ct[geneselect_reactives$selectRows(), "colorby"] <- "in highlighted gene set"
        ct$colorby <- factor(ct$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))

        ct$label <- idToLabel(rownames(ct), selectmatrix_reactives$getExperiment())
        ct$label[!rownames(ct) %in% c(rownames(fct), geneselect_reactives$selectRows())] <- NA
      })
      ct
    })

    # Display the data as a table alongside

    simpletable("volcanotable", downloadMatrix = contrast_reactives$labelledContrastsTable, displayMatrix = contrast_reactives$linkedLabelledContrastsTable, filename = "volcano", rownames = FALSE, pageLength = 10)
  })
}
