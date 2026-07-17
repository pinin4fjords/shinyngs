pca_modal <- list(id = "pca", title = "Principal components analysis")

# Default for the "Number of loadings to examine" slider, shared between the
# UI default and the server-side fallback used before the debounced reactive
# has a value from the client.
PCA_DEFAULT_N_LOADINGS <- 10

#' The input function of the pca module
#'
#' This provides the form elements to control the pca display, derived from the
#' \code{selectmatrix}, \code{scatterplotcontrols}, and \code{scatterplot}
#' modules.
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
#' pcaInput("pca", eselist)
#'
#' # Almost certainly used via application creation
#'
#' if (interactive()) {
#'   app <- prepareApp("pca", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
pcaInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("pca"), eselist)

  pca_filters <- list(sliderInput(ns("n_loadings"), "Number of loadings to examine", min = 2, max = 100, value = PCA_DEFAULT_N_LOADINGS))

  # Output sets of fields in their own containers

  fieldSets(ns("fieldset"), list(
    principal_component_analysis = pca_filters, scatter_plot = list(scatterplotcontrolsInput(ns("pca"), allow_3d = TRUE), groupbyInput(ns("pca"))),
    expression = expression_filters, export = list(simpletableInput(ns("components"), tabletitle = "Components"), simpletableInput(ns("loading"), tabletitle = "Loading"))
  ))
}

#' The output function of the pca module
#'
#' This provides a shiny \code{tabsetPanel} with \code{tabPanel}s for both
#' components and loading plots.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' pcaOutput("pca")
#'
#' # Almost certainly used via application creation
#'
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#'
#' if (interactive()) {
#'   app <- prepareApp("pca", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
pcaOutput <- function(id) {
  ns <- NS(id)

  list(modalInput(ns(pca_modal$id), "help", "help"), h3("Principal components analysis"), tabsetPanel(
    tabPanel("Components plot", scatterplotOutput(ns("pca")), simpletableOutput(ns("components"))),
    tabPanel("Loadings plot", list(scatterplotOutput(ns("loading")), simpletableOutput(ns("loading"), tabletitle = "Loadings")))
  ))
}

#' The server function of the pca module
#'
#' This module calculates a PCA and formats components and loadings for display.
#' It uses a common set of controls, generated with the
#' \code{scatterplotcontrols} module, to power scatter plots for both components
#' and loadings produced by the \code{scatterplots} module.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' Matrix and UI selection elements provided by the selectmatrix module
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
#'   pca("pca", eselist)
#'   app <- prepareApp("pca", eselist)
#'   shiny::shinyApp(ui = app$ui, server = app$server)
#' }
#'
pca <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(pca_modal$id, pca_modal$title)

    selectmatrix_reactives <- selectmatrix("pca", eselist, var_n = 1000, select_genes = TRUE, provide_all_genes = TRUE, default_gene_select = "variance", select_meta = FALSE)

    # Call the groupby module to define sample groups and group colors

    groupby_reactives <- groupby("pca", eselist = eselist, group_label = "Color by", selectColData = selectmatrix_reactives$selectColData)

    # Make a common set of controls to be used for components and loadings plots

    scatterplotcontrols_reactives <- scatterplotcontrols("pca", pcaMatrix)

    # Create a PCA plot using the controls supplied by scatterplotcontrols module and unpacked above for both PCA and loading

    scatterplot("pca",
      getDatamatrix = pcaMatrix, getThreedee = scatterplotcontrols_reactives$getThreedee, getXAxis = scatterplotcontrols_reactives$getXAxis,
      getYAxis = scatterplotcontrols_reactives$getYAxis, getZAxis = scatterplotcontrols_reactives$getZAxis, getShowLabels = scatterplotcontrols_reactives$getShowLabels,
      getPointSize = scatterplotcontrols_reactives$getPointSize, getTitle = getComponentsTitle, colorBy = pcaColorBy, getPalette = groupby_reactives$getPalette
    )
    scatterplot("loading",
      getDatamatrix = loadingMatrix, getThreedee = scatterplotcontrols_reactives$getThreedee, getXAxis = scatterplotcontrols_reactives$getXAxis,
      getYAxis = scatterplotcontrols_reactives$getYAxis, getZAxis = scatterplotcontrols_reactives$getZAxis, getShowLabels = scatterplotcontrols_reactives$getShowLabels,
      getPointSize = scatterplotcontrols_reactives$getPointSize, getTitle = getLoadingTitle, getLabels = getLoadLabels
    )

    # Simple title functions

    getComponentsTitle <- reactive({
      paste("Components plot for PCA on matrix:", tolower(selectmatrix_reactives$matrixTitle()))
    })

    getLoadingTitle <- reactive({
      paste("Loading plot for PCA on matrix:", tolower(selectmatrix_reactives$matrixTitle()))
    })

    # Make a matrix of values to the PCA

    pcaMatrix <- reactive({
      withProgress(message = "Making PCA matrix", value = 0, {
        fraction_explained <- calculatePCAFractionExplained()
        plotdata <- data.frame(pca()$x)
        colnames(plotdata) <- paste0(colnames(plotdata), ": ", fraction_explained, "%")
        plotdata
      })
    })

    pcaDisplayMatrix <- reactive({
      pcam <- pcaMatrix()
      pcam <- apply(pcam[, seq_len(min(ncol(pcam), 10)), drop = FALSE], 2, function(x) round(x, 2))
      pcam
    })

    pcaColorBy <- reactive({
      if (is.null(groupby_reactives$getGroupby())) {

      } else {
        pcb <- na.replace(selectmatrix_reactives$selectColData()[[groupby_reactives$getGroupby()]], "N/A")
        factor(pcb, levels = unique(pcb))
      }
    })

    # Run the PCA. Cached on the input matrix so tweaking display-only controls
    # (loadings count, axis choice, coloring) doesn't re-run prcomp().

    pca <- reactive({
      pcamatrix <- selectmatrix_reactives$selectMatrix()
      withProgress(message = "Running principal component analysis", value = 0, {
        validateOrCatch(runPCA(pcamatrix))
      })
    }) %>% bindCache(selectmatrix_reactives$selectMatrix())

    # Fractional variance for each component

    calculatePCAFractionExplained <- reactive({
      pca <- pca()
      round((pca$sdev)^2 / sum(pca$sdev^2), 3) * 100
    })

    # Loading matrix

    loadingMatrix <- reactive({
      getLoadings()$load
    })

    selectedComponents <- reactive({
      c(scatterplotcontrols_reactives$getXAxis(), scatterplotcontrols_reactives$getYAxis(), scatterplotcontrols_reactives$getZAxis())
    })

    # Debounce the loadings-count slider so dragging it doesn't refetch the
    # loadings on every tick. Fall back to the slider's default while
    # input$n_loadings hasn't reached the server yet - a debounced reactive's
    # first value is primed synchronously, before the client has necessarily
    # sent its initial slider value, and seq_len() below errors on NULL.

    getNLoadings <- reactive({
      if (is.null(input$n_loadings)) PCA_DEFAULT_N_LOADINGS else input$n_loadings
    }) %>% debounce(300)

    # Fetch the loadings

    getLoadings <- reactive({
      withProgress(message = "Fetching loadings", value = 0, {
        rot <- pca()$rotation
        fraction_explained <- calculatePCAFractionExplained()
        colnames(rot) <- paste0(colnames(rot), ": ", fraction_explained, "%")

        loaded_rows <- Reduce(union, lapply(selectedComponents(), function(pc) rownames(rot)[order(abs(rot[, pc]), decreasing = TRUE)[seq_len(getNLoadings())]]))

        # Also return a table with the loadings converted to fractions

        aload <- abs(rot)
        fractions <- sweep(aload, 2, colSums(aload), "/")

        list(load = rot[loaded_rows, ], fraction = fractions[loaded_rows, ])
      })
    })

    # Take the loadings and format them for display

    makeLoadingTable <- reactive({
      load <- getLoadings()$load[, selectedComponents()]
      colnames(load) <- paste(colnames(load), "loading")

      fraction <- getLoadings()$fraction[, selectedComponents()]
      colnames(fraction) <- paste(colnames(fraction), "loading fraction")

      interleaveColumns(load, fraction)
    })

    # Make a version of the loading table for display with rounded values and links

    makeDisplayLoadingTable <- reactive({
      linkMatrix(labelMatrix(data.frame(signif(makeLoadingTable(), 5), check.names = FALSE), selectmatrix_reactives$getExperiment()), url_roots = eselist@url_roots)
    })

    makeDownloadLoadingTable <- reactive({
      labelMatrix(data.frame(makeLoadingTable(), check.names = FALSE), selectmatrix_reactives$getExperiment())
    })

    # Make labels for the laoding plot detailing the percent contributions to components etc

    getLoadLabels <- reactive({
      load <- getLoadings()

      percent_contributions <- lapply(selectedComponents(), function(n) {
        paste0(paste(paste0("PC", n), round((load$fraction[, n] * 100), 3), sep = ": "), "%")
      })
      percent_contributions$sep <- "<br />"

      loadlabels <- paste(idToLabel(rownames(load$fraction), selectmatrix_reactives$getExperiment()), do.call(paste, percent_contributions), sep = "<br />")
    })

    # server = FALSE: column headers here embed the percent variance explained,
    # which changes with every PCA re-run, so DT's server-side paging can race
    # a debounced control change and fetch a page against headers that no
    # longer match (see simpletable()'s `server` argument).

    simpletable("components", downloadMatrix = pcaDisplayMatrix, displayMatrix = pcaDisplayMatrix, filename = "components", rownames = TRUE, server = FALSE)

    simpletable("loading", downloadMatrix = makeDownloadLoadingTable, displayMatrix = makeDisplayLoadingTable, filename = "pcaloading", rownames = FALSE, server = FALSE)
  })
}

#' Run a simple PCA analysis
#'
#' Common function for PCA-using parts of the app
#'
#' @param matrix Matrix (not logged)
#' @param do_log Boolean- apply log transform to input matrix?
#'
#' @return pca Output of the prcomp function
#'
#' @keywords shiny
#'
#' @examples
#' runPCA(mymatrix)
#'
runPCA <- function(matrix, do_log = TRUE) {
  if (ncol(matrix) < 2) {
    stop("PCA requires at least 2 samples; only ", ncol(matrix), " were supplied.")
  }

  if (do_log) {
    matrix <- log2(matrix + 1)
  }

  matrix <- matrix[rowsWithMultipleValues(matrix), , drop = FALSE]

  if (nrow(matrix) == 0) {
    stop("PCA requires at least one feature with variable values across samples; none remain after filtering.")
  }

  prcomp(t(as.matrix(matrix)), scale. = TRUE)
}

#' Run PCA on a given matrix, expected to be variance stabilised (at least
#' log-transformed)
#'
#' @param matrix Simple matrix with genes by row and samples by column
#' @param ntop Number of most variable genes to use
#'
#' @export
#'
#' @return a list with keys 'coords' and 'percentVar' providing PCA coordinates
#'   and fractional variance contributions, respectively.

compilePCAData <- function(matrix, ntop = NULL) {
  if (is.null(ntop)) {
    select <- seq_len(nrow(matrix))
  } else {
    select <- selectVariableGenes(matrix = matrix, ntop = ntop)
  }

  # perform a PCA on the data in assay(x) for the selected genes
  pca <- runPCA(matrix[select, , drop = FALSE], do_log = FALSE)

  # the contribution to the total variance for each component
  percentVar <- calculatePCAFractionExplained(pca)

  list(coords = data.frame(pca$x), percentVar = percentVar)
}

#' Extract the percent variance from a PCA analysis
#'
#' @param pca An output from \code{prcomp}
#'
#' @return output vector of percentages
#'
#' @keywords shiny
#'
#' @examples
#' calculatePCAFractionExplained(pca)
#'
calculatePCAFractionExplained <- function(pca) {
  round((pca$sdev)^2 / sum(pca$sdev^2), 3) * 100
}
