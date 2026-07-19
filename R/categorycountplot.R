#' The input function of the categorycountplot module
#'
#' Provides 'Count by'/'Split by' selectors and a bar mode toggle for tallying
#' rows of a data frame by a categorical column, plus an export table for the
#' resulting counts.
#'
#' @param id Submodule namespace
#'
#' @return A list of controls that can be added to a UI definition
#'
#' @keywords shiny
#'
#' @examples
#' categorycountplotInput("categorycount")
#'
categorycountplotInput <- function(id) {
  ns <- NS(id)

  list(uiOutput(ns("fields")), simpletableInput(ns("table"), tabletitle = "Category counts"))
}

#' The output function of the categorycountplot module
#'
#' @param id Submodule namespace
#'
#' @return A list of elements that can be included in a panel
#'
#' @keywords shiny
#'
#' @examples
#' categorycountplotOutput("categorycount")
#'
categorycountplotOutput <- function(id) {
  ns <- NS(id)

  list(
    shinycssloaders::withSpinner(plotlyOutput(ns("plot"), height = "500px"), color = shinyngsSpinnerColor()),
    simpletableOutput(ns("table"), tabletitle = "Counts", spinner = TRUE)
  )
}

#' The server function of the categorycountplot module
#'
#' Lets the user tally rows of a data frame by a categorical column,
#' optionally split by a second, rendered via \code{\link{plotly_count_barplot}}.
#' Identifier-like columns (e.g. a gene or sample ID/name, where almost every
#' row has its own distinct value) are excluded, since there would be little
#' to count beyond '1' per row.
#'
#' @param id Module namespace
#' @param getAnnotation Reactive supplying a data frame (one row per feature or
#'   sample) whose categorical (non-numeric) columns are offered for counting
#' @param filename Filename stem for the exported counts table/plot image
#'
#' @keywords shiny
#'
#' @examples
#' categorycountplot("categorycount", getAnnotation = reactive(mtcars))
#'
categorycountplot <- function(id, getAnnotation, filename = "categorycounts") {
  moduleServer(id, function(input, output, session) {
    getCategoricalFields <- reactive({
      meta <- getAnnotation()

      fields <- names(meta)[!vapply(meta, is.numeric, logical(1))]
      non_na_counts <- vapply(fields, function(f) sum(!is.na(meta[[f]])), integer(1))
      cardinality <- vapply(fields, function(f) length(unique(stats::na.omit(meta[[f]]))), integer(1))

      # Exclude fields with no non-missing values (nothing to count), and identifier-like fields (e.g. a gene/sample ID or name) where most
      # values are near-unique and counts would mostly be 1

      fields <- fields[non_na_counts > 0 & cardinality <= non_na_counts / 2]

      # Order by ascending cardinality, so the most 'category-like' fields (fewest distinct non-missing values) are offered, and selected by
      # default, first

      fields[order(cardinality[fields])]
    })

    output$fields <- renderUI({
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

    getFill <- reactive({
      validate(need(input$fill, "Waiting for split-by selection"))
      if (input$fill == "none") NULL else input$fill
    })

    getBarmode <- reactive({
      if (is.null(input$barmode)) "group" else input$barmode
    })

    # A long-format count table backing the plot, for display/download

    countTable <- reactive({
      meta <- getAnnotation()
      validate(need(input$category, "Waiting for category selection"))

      fill <- getFill()
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

    output$plot <- renderPlotly({
      meta <- getAnnotation()
      validate(need(input$category, "Waiting for category selection"))

      plotly_count_barplot(meta, input$category, getFill(), barmode = getBarmode()) %>%
        shinyngsPlotlyConfig(filename, format = session$userData$plotFormat())
    })

    simpletable("table", displayMatrix = countTable, filename = filename, rownames = FALSE)
  })
}
