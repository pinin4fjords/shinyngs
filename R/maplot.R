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
  ns <- NS(id)

  # Only consider experiments that actually have p-values to use in a volcano plot

  expression_filters <- selectmatrixInput(ns("expression"), eselist)

  # If there's only one experiment, then the expression filters will just be hidden fields, and there's no point in creating an empty fieldset for them

  fieldsets <- list(contrasts = list(contrastsInput(ns("differential"))))
  if (length(eselist) > 1 || length(assays(eselist[[1]])) > 1) {
    fieldsets$expression_matrix <- expression_filters
  }

  fieldsets <- c(fieldsets, list(
    scatter_plot = scatterplotInput(ns("ma")), highlight_points = geneselectInput(ns("ma")),
    export = simpletableInput(ns("matable"))
  ))

  inputs <- list(fieldSets(ns("fieldset"), fieldsets))

  if (length(eselist) == 1 && length(assays(eselist[[1]])) == 1) {
    inputs <- pushToList(inputs, expression_filters)
  }

  inputs
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
  ns <- NS(id)

  list(
    modalInput(ns(maplot_modal$id), "help", "help"),
    h3("MA plot"), scatterplotOutput(ns("ma")), htmlOutput(ns("matable"))
  )
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

    # Call the selectmatrix module and hold on to the reactives it sends back

    selectmatrix_reactives <- selectmatrix("expression", eselist, var_n = 1000, select_samples = FALSE, select_genes = FALSE, provide_all_genes = TRUE)

    # Pass the matrix to the contrasts module for processing

    contrast_reactives <- contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = FALSE)

    # Call the geneselect module (indpependently of selectmatrix) to generate sets of genes to highlight

    geneselect_reactives <- geneselect("ma", eselist = eselist, getExperiment = selectmatrix_reactives$getExperiment, getAssay = selectmatrix_reactives$getAssay, provide_all = FALSE, provide_none = TRUE)

    output$matable <- renderUI({
      ns <- session$ns

      simpletableOutput(ns("matable"), tabletitle = paste("Plot data for contrast", contrast_reactives$getSelectedContrastNames()[[1]], sep = ": "))
    })

    # Pass the matrix to the scatterplot module for display

    scatterplot("ma", getDatamatrix = maTable, getTitle = getTitle, allow_3d = FALSE, getLabels = maLabels, x = 1, y = 2, colorBy = colorBy, getLines = plotLines)


    # Make a title by selecting the single contrast name of the single filter set

    getTitle <- reactive({
      contrast_names <- contrast_reactives$getSelectedContrastNames()
      contrast_names[[1]][[1]]
    })

    # Make a set of dashed lines to overlay on the plot representing thresholds

    plotLines <- reactive({
      withProgress(message = "Calculating lines", value = 0, {
        mat <- maTable()

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
        if (fccard %in% c("> or <-", "< and >-")) {
          lines
        } else if (fccard == "<" && sign(fclim) == "-1") {
          droplevels(lines[c(1, 2), ])
        } else {
          droplevels(lines[c(3, 4), ])
        }
      })
    })


    # Extract labels from the volcano table

    maLabels <- reactive({
      fct <- maTable()
      fct$label
    })

    # Extract a vector use to make colors by group

    colorBy <- reactive({
      fct <- maTable()
      fct$colorby
    })

    # Make a table of values to use in the volcano plot. Round the values to save space in the JSON

    maTable <- reactive({
      withProgress(message = "Compiling fold change plot data", value = 0, {
        sct <- contrast_reactives$selectedContrastsTables()
        ct <- sct[[1]][[1]]

        matable <- data.frame(round(log10(rowMeans(ct[, 1:2])), 3), round(sign(ct[["Fold change"]]) *
          log2(abs(ct[["Fold change"]])), 3), row.names = rownames(ct), check.names = FALSE)
        colnames(matable) <- c("log(10) mean expression", paste0("log(2) fold change [source scale: ", contrast_reactives$getFoldChangeScale(), "]"))

        fct <- contrast_reactives$filteredContrastsTables()[[1]][[1]]
        matable$colorby <- "hidden"
        matable[rownames(fct), "colorby"] <- "match contrast filters"
        matable[geneselect_reactives$selectRows(), "colorby"] <- "in highlighted gene set"
        matable$colorby <- factor(matable$colorby, levels = c("hidden", "match contrast filters", "in highlighted gene set"))

        matable$label <- idToLabel(rownames(matable), selectmatrix_reactives$getExperiment())
        matable$label[!rownames(matable) %in% c(rownames(fct), geneselect_reactives$selectRows())] <- NA
      })
      matable
    })

    # Display the data as a table alongside

    simpletable("matable", downloadMatrix = contrast_reactives$labelledContrastsTable, displayMatrix = contrast_reactives$linkedLabelledContrastsTable, filename = "ma", rownames = FALSE, pageLength = 10)
  })
}
