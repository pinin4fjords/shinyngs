differentialsummary_modal <- list(id = "differentialsummary", title = "Differential summary")

#' Input function for the differential summary module
#'
#' @param id Module namespace.
#' @param eselist ExploratorySummarizedExperimentList object.
#'
#' @return Controls for expression data, contrast filtering, and table export.
#' @keywords shiny
differentialsummaryInput <- function(id, eselist) {
  ns <- NS(id)
  fieldSets(ns("fieldset"), list(
    expression = selectmatrixInput(ns("differentialsummary"), eselist),
    contrasts = contrastsInput(ns("differentialsummary")),
    export = simpletableInput(ns("table"), "Differential summary")
  ))
}

#' Output function for the differential summary module
#'
#' @param id Module namespace.
#'
#' @return Differential count plot and its backing table.
#' @keywords shiny
differentialsummaryOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    "Differential summary",
    uiOutput(ns("parameters")),
    uiOutput(ns("plot_ui")),
    h4("Summary data"),
    simpletableOutput(ns("table")),
    help = modalInput(ns(differentialsummary_modal$id), "help", "help")
  )
}

#' Server function for the differential summary module
#'
#' @param id Module namespace.
#' @param eselist ExploratorySummarizedExperimentList object.
#'
#' @keywords shiny
differentialsummary <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(differentialsummary_modal$id, differentialsummary_modal$title)

    selectmatrix_reactives <- selectmatrix(
      "differentialsummary", eselist,
      var_n = 1000, select_samples = FALSE, select_genes = TRUE,
      provide_all_genes = TRUE, select_meta = FALSE
    )
    contrast_reactives <- contrasts(
      "differentialsummary", eselist = eselist,
      selectmatrix_reactives = selectmatrix_reactives,
      multiple = TRUE, select_all_contrasts = TRUE
    )

    getDifferentialSummary <- contrast_reactives$makeDifferentialSetSummary

    output$parameters <- renderUI({
      query_strings <- contrast_reactives$getQueryStrings()
      helpText(HTML(query_strings[1]))
    })

    output$plot_ui <- renderUI({
      summary <- getDifferentialSummary()
      height <- min(1000, max(420, nrow(summary) * 38 + 160))
      shinyngsSpinner(plotlyOutput(session$ns("plot"), height = paste0(height, "px")))
    })

    getDifferentialSummaryPlot <- reactive({
      interactive_differential_summary(getDifferentialSummary())
    }) %>% bindCache(getDifferentialSummary())

    output$plot <- renderPlotly({
      getDifferentialSummaryPlot() %>%
        shinyngsPlotlyConfig("differential_summary", format = session$userData$plotFormat())
    })

    simpletable(
      "table",
      downloadMatrix = getDifferentialSummary,
      displayMatrix = getDifferentialSummary,
      filter = "none", filename = "differential_summary",
      rownames = FALSE, server = FALSE
    )
  })
}

#' Plot differential feature counts across contrasts
#'
#' Draws up-regulated counts to the right and down-regulated counts to the
#' left while displaying absolute count labels on both sides of the axis.
#'
#' @param summary_table A table returned by a contrasts module's
#'   \code{makeDifferentialSetSummary} reactive.
#' @param title Plot title.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' interactive_differential_summary(data.frame(
#'   Variable = c("condition", "condition"),
#'   `group 1` = c("control", "control"),
#'   `group 2` = c("treated_a", "treated_b"),
#'   `Differential genes (up)` = c(120, 45),
#'   `Differential genes (down)` = c(80, 60),
#'   `Differential genes (total)` = c(200, 105),
#'   check.names = FALSE
#' ))
interactive_differential_summary <- function(summary_table, title = "Differential features across contrasts") {
  up_column <- grep("\\(up\\)$", colnames(summary_table), value = TRUE)
  down_column <- grep("\\(down\\)$", colnames(summary_table), value = TRUE)
  group_1_column <- intersect(c("group 1", "Condition 1"), colnames(summary_table))
  group_2_column <- intersect(c("group 2", "Condition 2"), colnames(summary_table))

  if (!"Variable" %in% colnames(summary_table) || length(group_1_column) != 1 || length(group_2_column) != 1 || length(up_column) != 1 || length(down_column) != 1) {
    stop("interactive_differential_summary(): table does not contain one contrast definition and one pair of up/down count columns")
  }
  if (nrow(summary_table) == 0) {
    stop("interactive_differential_summary(): no contrasts to plot")
  }

  up <- suppressWarnings(as.numeric(summary_table[[up_column]]))
  down <- suppressWarnings(as.numeric(summary_table[[down_column]]))
  if (any(!is.finite(up)) || any(!is.finite(down)) || any(up < 0) || any(down < 0)) {
    stop("interactive_differential_summary(): differential counts must be finite and non-negative")
  }

  contrast <- paste0(
    prettify_variable_name(as.character(summary_table$Variable)), ": ",
    summary_table[[group_2_column]], " vs ", summary_table[[group_1_column]]
  )
  escaped_contrast <- htmltools::htmlEscape(contrast)
  up_hover <- paste0("<b>", escaped_contrast, "</b><br>Up: ", format(up, big.mark = ",", scientific = FALSE))
  down_hover <- paste0("<b>", escaped_contrast, "</b><br>Down: ", format(down, big.mark = ",", scientific = FALSE))

  limit <- max(c(up, down, 1))
  tick_values <- pretty(c(-limit, limit))

  plot_ly() %>%
    add_bars(
      x = -down, y = contrast, name = "Down", orientation = "h",
      marker = list(color = DIRECTION_COLORS[["Down"]]), text = down_hover,
      textposition = "none", hovertemplate = "%{text}<extra></extra>"
    ) %>%
    add_bars(
      x = up, y = contrast, name = "Up", orientation = "h",
      marker = list(color = DIRECTION_COLORS[["Up"]]), text = up_hover,
      textposition = "none", hovertemplate = "%{text}<extra></extra>"
    ) %>%
    layout(
      title = htmltools::htmlEscape(title), barmode = "relative",
      xaxis = list(
        title = "Differential features (down \u2190 | \u2192 up)",
        tickvals = tick_values,
        ticktext = format(abs(tick_values), big.mark = ",", scientific = FALSE, trim = TRUE),
        zeroline = TRUE, zerolinecolor = "#595959"
      ),
      yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = rev(contrast)),
      legend = list(orientation = "h", x = 0, y = -0.18),
      margin = list(l = 40, r = 20, b = 90, t = 70)
    )
}
