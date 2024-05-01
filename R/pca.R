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
#' pcaInput("pca", ese, group_vars, default_groupvar, tructure(1:10, names = paste0("PC", 1:10)))
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("pca", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
pcaInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("pca"), eselist)

  pca_filters <- list(sliderInput(ns("n_loadings"), "Number of loadings to examine", min = 2, max = 100, value = 10))

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
#' data(zhangneurons)
#' app <- prepareApp("pca", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
pcaOutput <- function(id) {
  ns <- NS(id)

  list(modalInput(ns("pca"), "help", "help"), modalOutput(ns("pca"), "Principal components analysis", includeMarkdown(system.file("inlinehelp", "pca.md",
    package = packageName()
  ))), h3("Principal components analysis"), tabsetPanel(
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
#' This function is not called directly, but rather via callModule() (see
#' example).
#'
#' Matrix and UI selection elements provided by the selectmatrix module
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
#' callModule(pca, "pca", eselist)
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("pca", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
pca <- function(input, output, session, eselist) {
  unpack.list(callModule(selectmatrix, "pca", eselist, var_n = 1000, select_genes = TRUE, provide_all_genes = TRUE, default_gene_select = "variance", select_meta = FALSE))

  # Call the groupby module to define sample groups and group colors

  unpack.list(callModule(groupby, "pca", eselist = eselist, group_label = "Color by", selectColData = selectColData))

  # Make a common set of controls to be used for components and loadings plots

  unpack.list(callModule(scatterplotcontrols, "pca", pcaMatrix))

  # Create a PCA plot using the controls supplied by scatterplotcontrols module and unpacked above for both PCA and loading

  callModule(scatterplot, "pca",
    getDatamatrix = pcaMatrix, getThreedee = getThreedee, getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getShowLabels = getShowLabels,
    getPointSize = getPointSize, getTitle = getComponentsTitle, colorBy = pcaColorBy, getPalette = getPalette
  )
  callModule(scatterplot, "loading",
    getDatamatrix = loadingMatrix, getThreedee = getThreedee, getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis,
    getShowLabels = getShowLabels, getPointSize = getPointSize, getTitle = getLoadingTitle, getLabels = getLoadLabels
  )

  # Simple title functions

  getComponentsTitle <- reactive({
    paste("Components plot for PCA on matrix:", tolower(matrixTitle()))
  })

  getLoadingTitle <- reactive({
    paste("Loading plot for PCA on matrix:", tolower(matrixTitle()))
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
    pcam <- apply(pcam[, 1:min(ncol(pcam), 10)], 2, function(x) round(x, 2))
    pcam
  })

  pcaColorBy <- reactive({
    if (is.null(getGroupby())) {

    } else {
      pcb <- na.replace(selectColData()[[getGroupby()]], "N/A")
      factor(pcb, levels = unique(pcb))
    }
  })

  # Run the PCA

  pca <- reactive({
    pcamatrix <- selectMatrix()
    withProgress(message = "Running principal component analysis", value = 0, {
      runPCA(pcamatrix)
    })
  })

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
    c(getXAxis(), getYAxis(), getZAxis())
  })

  # Fetch the loadings

  getLoadings <- reactive({
    withProgress(message = "Fetching loadings", value = 0, {
      rot <- pca()$rotation
      fraction_explained <- calculatePCAFractionExplained()
      colnames(rot) <- paste0(colnames(rot), ": ", fraction_explained, "%")

      loaded_rows <- Reduce(union, lapply(selectedComponents(), function(pc) rownames(rot)[order(abs(rot[, pc]), decreasing = TRUE)[1:input$n_loadings]]))

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
    linkMatrix(labelMatrix(data.frame(signif(makeLoadingTable(), 5), check.names = FALSE), getExperiment()), url_roots = eselist@url_roots)
  })

  makeDownloadLoadingTable <- reactive({
    labelMatrix(data.frame(makeLoadingTable(), check.names = FALSE), getExperiment())
  })

  # Make labels for the laoding plot detailing the percent contributions to components etc

  getLoadLabels <- reactive({
    load <- getLoadings()

    percent_contributions <- lapply(selectedComponents(), function(n) {
      paste0(paste(paste0("PC", n), round((load$fraction[, n] * 100), 3), sep = ": "), "%")
    })
    percent_contributions$sep <- "<br />"

    loadlabels <- paste(idToLabel(rownames(load$fraction), getExperiment()), do.call(paste, percent_contributions), sep = "<br />")
  })

  callModule(simpletable, "components", downloadMatrix = pcaDisplayMatrix, displayMatrix = pcaDisplayMatrix, filename = "components", rownames = TRUE)

  callModule(simpletable, "loading", downloadMatrix = makeDownloadLoadingTable, displayMatrix = makeDisplayLoadingTable, filename = "pcaloading", rownames = FALSE)
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
  if (do_log) {
    matrix <- log2(matrix + 1)
  }

  matrix <- matrix[apply(matrix, 1, function(x) length(unique(x))) > 1, ]

  prcomp(as.matrix(t(matrix), scale = T))
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

  if (is.null(ntop)){
    select <- 1:nrow(matrix)
  }else{
    select <- selectVariableGenes(matrix = matrix, ntop = ntop)
  }

  # perform a PCA on the data in assay(x) for the selected genes
  pca <- runPCA(matrix, do_log = FALSE)

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
