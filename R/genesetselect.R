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
  ), multiple = multiple), radioButtons(ns("overlapType"), withHelpIcon("Overlap type", "'Union' includes genes from any of the selected gene sets; 'intersect' includes only genes common to all of them."), c("union", "intersect")))
}

#' The server function of the genesetselect module
#'
#' The gene set module is for adding a gene set filter to displays. A
#' \code{\link[shiny]{selectizeInput}} is used for performance reasons,
#' providing an autocomplete field for selecting from a list that could stretch
#' to thousands of entries. This would be difficult to do client-side using a
#' standard select field.
#'
#' @param id Module namespace
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
#' geneset_functions <- genesetselect("heatmap", getExperiment)
#'
genesetselect <- function(id, eselist, getExperiment, multiple = TRUE, filter_by_type = FALSE, require_select = TRUE) {
  moduleServer(id, function(input, output, session) {
    # The gene-set picker is a server-side selectize, so a bookmarked selection
    # can't restore itself; stash it here and apply it when the list is next
    # populated (see updateGeneSetsList).

    restored_geneset <- NULL

    onRestore(function(state) {
      val <- bookmarkedInputValue(state, session, "geneSets")
      if (!is.null(val)) {
        restored_geneset <<- val
      }
    })

    # Fetch the gene sets keyed for the current experiment's label field,
    # guarding against experiments with no gene sets available for that field

    getGeneSetsForLabelfield <- reactive({
      ese <- getExperiment()
      gene_sets <- eselist@gene_sets[[ese@labelfield]]
      validate(need(length(gene_sets) > 0, paste0("No gene sets available for identifier type '", ese@labelfield, "'")))
      gene_sets
    })

    # Allow user to select the type of gene set

    output$geneSetTypes <- renderUI({
      if (filter_by_type) {
        gene_set_types <- names(getGeneSetsForLabelfield())
        ns <- session$ns
        selectInput(ns("geneSetTypes"), "Gene set type", gene_set_types, selected = gene_set_types[1])
      }
    })

    # Reactive to fetch the gene set types (if used)

    getGeneSetTypes <- reactive({
      if (!filter_by_type) {
        names(getGeneSetsForLabelfield())
      } else {
        validate(need(input$geneSetTypes, "Waiting for gene set type"))
        input$geneSetTypes
      }
    })

    # Get a list of names to show for the gene sets

    getGeneSetNames <- reactive({
      gene_sets <- getGeneSets()

      structure(paste(unlist(lapply(seq_along(gene_sets), function(x) paste(x, seq_along(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(
        names(gene_sets),
        function(settype) paste0(prettifyGeneSetName(names(gene_sets[[settype]])), " (", settype, ")")
      )))
    })

    # A reactive for relating codes back to gene set IDs

    getGeneSetCodesByIDs <- reactive({
      gene_sets <- getGeneSets()
      structure(paste(unlist(lapply(seq_along(gene_sets), function(x) paste(x, seq_along(gene_sets[[x]]), sep = "-")))), names = unlist(lapply(
        names(gene_sets),
        function(settype) names(gene_sets[[settype]])
      )))
    })

    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list.  This reactive must be called by
    # the calling module.

    updateGeneSetsList <- reactive({
      selected <- restored_geneset
      restored_geneset <<- NULL
      updateSelectizeInput(session, "geneSets", choices = getGeneSetNames(), selected = selected, server = TRUE)
    })

    # Get gene sets with the proper label field keying

    getGeneSets <- reactive({
      gene_sets <- getGeneSetsForLabelfield()
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
  })
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
