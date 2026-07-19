rowmetatable_modal <- list(id = "rowmetatable", title = "Experimental data table")

#' The UI input function of the rowmetatable module
#'
#' This module produces a simple table of the row metadata (accessed via
#' \code{mcols}) in a SummarizedExperiment object. If more than one of these
#' objects were specified, a select box should appear to pick the desired one for display.
#'
#' Leverages the \code{simpletable} module
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
#' experimentableInput("experiment", eselist)
#'
rowmetatableInput <- function(id, eselist) {
  ns <- NS(id)

  description <- "This is the metadata associated with the rows (e.g. genes) of this study."

  list(
    selectmatrixInput(ns("rowmetatable"), eselist = eselist),
    simpletableInput(ns("rowmetatable"), tabletitle = "Annotation"),
    uiOutput(ns("categorycount_fields")),
    simpletableInput(ns("categorycount"), tabletitle = "Category counts")
  )
}

#' The output function of the rowmetatable module
#'
#' This module produces a simple table of the \code{colData()} in an
#' ExploratorySummarizedExperiment object. If more than one of these objects
#' were specified, a select box should appear to pick the desired one for
#' display.
#'
#' Leverages the \code{simpletable} module
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' rowmetatableOutput("experiment")
#'
rowmetatableOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    "Row metadata",
    tabsetPanel(
      tabPanel("Table", simpletableOutput(ns("rowmetatable"))),
      tabPanel("Category counts", list(
        shinycssloaders::withSpinner(plotlyOutput(ns("categorycountplot"), height = "500px"), color = shinyngsSpinnerColor()),
        simpletableOutput(ns("categorycount"), tabletitle = "Counts", spinner = TRUE)
      ))
    ),
    help = modalInput(ns(rowmetatable_modal$id), "help", "help")
  )
}

#' The server function of the rowmetatable module
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
#' rowmetatable("rowmetatable", eselist)
#'
rowmetatable <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(rowmetatable_modal$id, rowmetatable_modal$title)

    selectmatrix_reactives <- selectmatrix("rowmetatable", eselist, select_assays = FALSE, select_samples = FALSE, select_genes = FALSE, select_meta = FALSE)

    getRowMeta <- reactive({
      meta <- selectmatrix_reactives$getAnnotation()
      meta
    })

    getLinkedRowMeta <- reactive({
      meta <- getRowMeta()
      colnames(meta) <- prettifyVariablename(colnames(meta))
      linkMatrix(meta, eselist@url_roots)
    })

    simpletable("rowmetatable", displayMatrix = getLinkedRowMeta, downloadMatrix = getRowMeta, filename = "rowmeta", rownames = TRUE, pageLength = 10)

    # Category counts: tally rows of the annotation table by a chosen categorical column, optionally split by a second

    getCategoricalFields <- reactive({
      meta <- getRowMeta()

      # Exclude the ID field (unique per row, so never useful to count by) and fields with no non-missing values (nothing to count)

      fields <- setdiff(names(meta)[!vapply(meta, is.numeric, logical(1))], selectmatrix_reactives$getIdField())
      fields <- fields[vapply(fields, function(f) any(!is.na(meta[[f]])), logical(1))]

      # Order by ascending cardinality, so the most 'category-like' fields (fewest distinct non-missing values) are offered, and selected by default, first

      cardinality <- vapply(fields, function(f) length(unique(stats::na.omit(meta[[f]]))), integer(1))
      fields[order(cardinality)]
    })

    output$categorycount_fields <- renderUI({
      ns <- session$ns
      fields <- getCategoricalFields()

      validate(need(length(fields) > 0, "No categorical annotation fields available for counting"))

      field_choices <- structure(fields, names = prettifyVariablename(fields))

      list(
        selectInput(ns("category"), "Count by", field_choices),
        selectInput(ns("fill"), "Split by", c(`(none)` = "none", field_choices)),
        conditionalPanel(
          condition = paste0("input['", ns("fill"), "'] != 'none'"),
          selectInput(ns("barmode"), "Mode", choices = c("group", "stack"), selected = "group")
        )
      )
    })

    # NULL when no split-by field is selected

    getCategoryFill <- reactive({
      validate(need(input$fill, "Waiting for split-by selection"))
      if (input$fill == "none") NULL else input$fill
    })

    getCategoryBarmode <- reactive({
      if (is.null(input$barmode)) "group" else input$barmode
    })

    # A long-format count table backing the plot, for display/download

    categoryCountTable <- reactive({
      meta <- getRowMeta()
      validate(need(input$category, "Waiting for category selection"))

      fill <- getCategoryFill()
      countdata <- melt_matrix(countMatrixByCategory(meta, input$category, fill), varnames = c("fill", "category"), value.name = "count")

      if (is.null(fill)) {
        countdata <- countdata[, c("category", "count")]
      } else {
        colnames(countdata)[colnames(countdata) == "fill"] <- prettifyVariablename(fill)
      }
      colnames(countdata)[colnames(countdata) == "category"] <- prettifyVariablename(input$category)
      colnames(countdata)[colnames(countdata) == "count"] <- "Count"
      countdata
    })

    output$categorycountplot <- renderPlotly({
      meta <- getRowMeta()
      validate(need(input$category, "Waiting for category selection"))

      plotly_count_barplot(meta, input$category, getCategoryFill(), barmode = getCategoryBarmode()) %>%
        shinyngsPlotlyConfig("categorycount", format = session$userData$plotFormat())
    })

    simpletable("categorycount", displayMatrix = categoryCountTable, filename = "categorycounts", rownames = FALSE)
  })
}
