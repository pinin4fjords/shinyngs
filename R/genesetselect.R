#' The UI function of the genesetselect module
#'
#' The gene set module is for adding a gene set filter to displays. A
#' \code{\link[shiny]{selectizeInput}} is used for performance reasons,
#' providing an autocomplete field for selecting from a list that could stretch
#' to thousands of entries. This would be difficult to do client-side using a
#' standard select field.
#'
#' @param id Submodule namespace
#' @param multiple Boolean: should it be possible to select multiple gene sets?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' genesetselectInput("myid")
#'
genesetselectInput <- function(id, multiple = TRUE) {
  ns <- NS(id)

  tagList(uiOutput(ns("geneSetTypes")), selectizeInput(ns("geneSets"), "Gene sets", choices = NULL, options = list(
    placeholder = "Type a gene set keyword",
    maxItems = 5
  ), multiple = multiple), radioButtons(ns("overlapType"), "Overlap type", c("union", "intersect")))
}

#' The server function of the genesetselect module
#'
#' The gene set module is for adding a gene set filter to displays. A
#' \code{\link[shiny]{selectizeInput}} is used for performance reasons,
#' providing an autocomplete field for selecting from a list that could stretch
#' to thousands of entries. This would be difficult to do client-side using a
#' standard select field.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist An ExploratorySummarizedExperimentList with its gene_sets
#' slot set
#' @param getExperiment Accessor for returning an
#'   ExploratorySummarizedExperiment object, with 'labelfield' set in its slots
#' @param multiple Boolean: should it be possible to select multiple gene sets?
#' @param filter_by_type Provide a filter for gene set type?
#' @param require_select Require a gene set selection?
#'
#' @return output A list of two reactive functions which will be used by other
#' modules.
#'
#' @keywords shiny
#'
#' @examples
#' geneset_functions <- callModule(genesetselect, "heatmap", getExperiment)
#'
genesetselect <- function(input, output, session, eselist, getExperiment, multiple = TRUE, filter_by_type = FALSE, require_select = TRUE) {
  # Allow user to select the type of gene set

  output$geneSetTypes <- renderUI({
    if (filter_by_type) {
      ese <- getExperiment()

      gene_set_types <- names(eselist@gene_sets[[ese@labelfield]])
      ns <- session$ns
      selectInput(ns("geneSetTypes"), "Gene set type", gene_set_types, selected = gene_set_types[1])
    }
  })

  # Reactive to fetch the gene set types (if used)

  getGeneSetTypes <- reactive({
    ese <- getExperiment()
    if (!filter_by_type) {
      names(eselist@gene_sets[[ese@labelfield]])
    } else {
      validate(need(input$geneSetTypes, "Waiting for gene set type"))
      input$geneSetTypes
    }
  })

  # Get a list of names to show for the gene sets

  getGeneSetNames <- reactive({
    gene_sets <- getGeneSets()

    structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(
      names(gene_sets),
      function(settype) paste0(prettifyGeneSetName(names(gene_sets[[settype]])), " (", settype, ")")
    )))
  })

  # A reactive for relating codes back to gene set IDs

  getGeneSetCodesByIDs <- reactive({
    gene_sets <- getGeneSets()
    structure(paste(unlist(lapply(1:length(gene_sets), function(x) paste(x, 1:length(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(
      names(gene_sets),
      function(settype) names(gene_sets[[settype]])
    )))
  })

  # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list.  This reactive must be called by
  # the calling module.

  updateGeneSetsList <- reactive({
    updateSelectizeInput(session, "geneSets", choices = getGeneSetNames(), server = TRUE)
  })

  # Get gene sets with the proper label field keying

  getGeneSets <- reactive({
    ese <- getExperiment()
    gene_sets <- eselist@gene_sets[[ese@labelfield]]
    gene_set_types <- getGeneSetTypes()
    gene_sets[gene_set_types]
  })

  # Rerieve and validate the gene set selection

  getInputGeneSets <- reactive({
    if (require_select) {
      validate(need(input$geneSets, "Please select a gene set"))
    }

    if ((!multiple)) {
      validate(need(length(input$geneSets) == 1, "Please select a single gene set only"))
    }

    input$geneSets
  })

  # Return list of reactive expressions

  list(getGeneSetTypes = getGeneSetTypes, getGeneSets = getGeneSets, updateGeneSetsList = updateGeneSetsList, getGenesetNames = reactive({
    gene_sets <- getGeneSets()
    input_gene_sets <- getInputGeneSets()

    if (is.null(input_gene_sets)) {
      return(NULL)
    }

    unlist(lapply(input_gene_sets, function(pathcode) {
      pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
      names(gene_sets[[pathparts[1]]])[pathparts[2]]
    }))
  }), getGenesetTypes = reactive({
    gene_sets <- getGeneSets()
    input_gene_sets <- getInputGeneSets()

    unique(unlist(lapply(input_gene_sets, function(pathcode) {
      pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
      names(gene_sets)[pathparts[1]]
    })))
  }), getPathwayGenes = reactive({
    gene_sets <- getGeneSets()
    input_gene_sets <- getInputGeneSets()

    gene_sets <- getGeneSets()
    path_gene_sets <- lapply(input_gene_sets, function(pathcode) {
      pathparts <- unlist(lapply(strsplit(pathcode, "-"), as.numeric))
      gene_sets[[pathparts[1]]][[pathparts[2]]]
    })

    if (input$overlapType == "union") {
      # Use c to preserve names

      Reduce(c, path_gene_sets)
    } else {
      # Again- this is more than a simple Reduce(intersect because of the need to preserve names

      path_gene_sets[[1]][Reduce(intersect, lapply(path_gene_sets, names))]
    }
  }), updateGeneset = reactive({
    query <- parseQueryString(session$clientData$url_search)
    geneset_codes <- getGeneSetCodesByIDs()
    validate(need(query$geneset %in% names(geneset_codes), "Invalid gene set ID entered"))

    geneset_code <- getGeneSetCodesByIDs()[query$geneset]
    updateSelectizeInput(session, "geneSets", selected = geneset_code, choices = getGeneSetNames(), server = TRUE)
  }))
}

#' Prettify gene set names like those from MSigDB
#'
#' @param gsn Gene set name like 'KEGG_GLYCOLYSIS_GLUCONEOGENESIS'
#'
#' @return output Prettified version
#' @export

prettifyGeneSetName <- function(gsn) {
  words <- strsplit(gsn, "_")

  unlist(lapply(words, function(w) paste(w[1], paste(tolower(w[-1]), collapse = " "))))
}
