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
#' volcanoplotInput("myid", eselist)
#'
#' # However, almost certainly called via application creation:
#'
#' data(zhangneurons)
#' app <- prepareApp("volcanoplot", zhangneurons)
#' shinyApp(ui = app$ui, server = app$server)
#'
volcanoplotInput <- function(id, eselist) {
  ns <- NS(id)

  # Only consider experiments that actually have p-values to use in a volcano plot

  eselist <- eselist[which(unlist(lapply(eselist, function(ese) {
    length(ese@contrast_stats) > 0
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
#' data(zhangneurons)
#' app <- prepareApp("volcanoplot", zhangneurons)
#' shinyApp(ui = app$ui, server = app$server)
#'
volcanoplotOutput <- function(id) {
  ns <- NS(id)

  list(modalInput(ns("volcanoplot"), "help", "help"), modalOutput(ns("volcanoplot"), "Volcano plots", includeMarkdown(system.file("inlinehelp", "volcanoplot.md",
    package = packageName()
  ))), h3("Volcano plot"), scatterplotOutput(ns("volcano")), htmlOutput(ns("volcanotable")))
}

#' The server function of the \code{volcanoplot} module
#'
#' This function is not called directly, but rather via callModule() (see
#' example). Essentially this just passes the results of \code{colData()}
#' applied to the specified SummarizedExperiment object to the
#' \code{simpletable} module
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' callModule(differentialtable, "differentialtable", eselist)
#'
#' # However, almost certainly called via application creation:
#'
#' data(zhangneurons)
#' app <- prepareApp("volcanoplot", zhangneurons)
#' shinyApp(ui = app$ui, server = app$server)
#'
volcanoplot <- function(input, output, session, eselist) {
  output$volcanotable <- renderUI({
    ns <- session$ns

    simpletableOutput(ns("volcanotable"), tabletitle = paste("Plot data for contrast", getSelectedContrastNames()[[1]][[1]], sep = ": "))
  })

  # Call the selectmatrix module and unpack the reactives it sends back

  selectmatrix_reactives <- callModule(selectmatrix, "expression", eselist,
    var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE,
    require_contrast_stats = TRUE
  )
  unpack.list(selectmatrix_reactives)

  # Pass the matrix to the contrasts module for processing

  unpack.list(callModule(contrasts, "differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE))

  # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight

  unpack.list(callModule(geneselect, "volcano", eselist = eselist, getExperiment = getExperiment, getAssay = getAssay, provide_all = FALSE, provide_none = TRUE))

  # Pass the matrix to the scatterplot module for display

  callModule(scatterplot, "volcano",
    getDatamatrix = volcanoTable, getTitle = getTitle, allow_3d = FALSE, getLabels = volcanoLabels, x = 1, y = 2, colorBy = colorBy,
    getLines = plotLines
  )

  # Make a title by selecting the single contrast name of the single filter set

  getTitle <- reactive({
    contrast_names <- getSelectedContrastNames()
    contrast_names[[1]][[1]]
  })

  # Make a set of dashed lines to overlay on the plot representing thresholds

  plotLines <- reactive({
    withProgress(message = "Calculating lines", value = 0, {
      vt <- volcanoTable()

      fclim <- getFoldChange()
      qvallim <- getQval()

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

      fccard <- getFoldChangeCard()

      if (fccard %in% c(">= or <= -", "<= and >= -")) {
        lines <- lines
      } else if (fccard == "<=" && sign(fclim) == "-1") {
        lines <- droplevels(lines[1, 2, 5, 6, ])
      } else {
        lines <- droplevels(lines[1:4, ])
      }
      lines
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

  colorBy <- reactive({
    vt <- volcanoTable()
    vt$colorby
  })

  # Make a table of values to use in the volcano plot. Round the values to save space in the JSON

  volcanoTable <- reactive({
    withProgress(message = "Compiling volcano plot data", value = 0, {
      sct <- selectedContrastsTables()
      ct <- sct[[1]][[1]]

      # q values of 0 cause trouble

      ct$`q value`[ct$`q value` == 0] <- min(ct$`q value`[ct$`q value` != 0]) / 10

      ct <- ct[, c("Fold change", "q value")]
      ct[["Fold change"]] <- round(sign(ct[["Fold change"]]) * log2(abs(ct[["Fold change"]])), 3)
      ct[["q value"]] <- round(-log10(ct[["q value"]]), 3)

      cont <- getSelectedContrasts()[[1]][[1]]
      colnames(ct) <- c(paste(paste0("(higher in ", cont[2], ")"), "log2(fold change)", paste0("(higher in ", cont[3], ")"), sep = "  "), "-log10(q value)")

      fct <- filteredContrastsTables()[[1]][[1]]
      ct$colorby <- "hidden"
      ct[rownames(fct), "colorby"] <- "match contrast filters"
      ct[selectRows(), "colorby"] <- "in highlighted gene set"
      ct$colorby <- factor(ct$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))

      ct$label <- idToLabel(rownames(ct), getExperiment())
      ct$label[!rownames(ct) %in% c(rownames(fct), selectRows())] <- NA
    })
    ct
  })

  # Display the data as a table alongside

  callModule(simpletable, "volcanotable",
    downloadMatrix = labelledContrastsTable, displayMatrix = linkedLabelledContrastsTable, filename = "volcano", rownames = FALSE,
    pageLength = 10
  )
}
