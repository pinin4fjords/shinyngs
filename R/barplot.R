#' Input function of the \code{barplot} module
#'
#' Generic module to isplay grouped, stacked or overlaid bars for a matrix
#'
#' @param id Submodule namespace
#' @param default_mode Default bar mode
#' @param allow_select Allow user to choose stack mode?
#'
#' @return A list of controls that can be added to a UI definition

barplotInput <- function(id, default_mode = "stack", allow_select = TRUE) {
  ns <- NS(id)

  if (allow_select) {
    selectInput(ns("barMode"), "Mode", choices = c("group", "stack", "overlay"), selected = default_mode)
  } else {
    hiddenInput(ns("barMode"), default_mode)
  }
}

#' Output function of the \code{barplot} module
#'
#' Generic module to isplay grouped, stacked or overlaid bars for a matrix
#'
#' @param id Submodule namespace
#' @param height Height of the plotting space in px
#'
#' @return A list of elements that can be included in a panel

barplotOutput <- function(id, height = "400") {
  ns <- NS(id)

  list(plotlyOutput(ns("barPlot"), height = paste0(height, "px")))
}

#' Server function of the \code{barplot} module
#'
#' Display grouped, stacked or overlaid bars for a matrix
#'
#' @param id Module namespace
#' @param getPlotmatrix Reactive supplying a matrix to plot
#' @param getYLabel Reactive supplying the Y axis label
#' @param barmode Bar mode: 'stack', 'group' or 'overlay'

barplot <- function(id, getPlotmatrix, getYLabel, barmode = "stack") {
  moduleServer(id, function(input, output, session) {
    output$barPlot <- renderPlotly({
      validate(need(input$barMode, "Waiting for bar mode"))

      plotly_barplot(getPlotmatrix(), barmode = input$barMode, ylab = getYLabel()) %>%
        shinyngsPlotlyConfig("barplot", format = session$userData$plotFormat())
    })
  })
}

#' Make a grouped, stacked or overlaid bar plot with \code{plot_ly()}
#'
#' Draws one bar trace per row of \code{plotmatrix}, coloured and legended by
#' row, with columns along the x axis. Used by the \code{barplot} module, and
#' by \code{\link{plotly_count_barplot}} to render feature-annotation category
#' counts.
#'
#' @param plotmatrix A matrix with series (colour/legend) as rows and x axis
#'   categories as columns.
#' @param barmode Bar mode: \code{"stack"} (default), \code{"group"} (dodged)
#'   or \code{"overlay"}.
#' @param ylab Y axis label
#' @param palette_name Valid R color palette name
#' @param title Plot title
#'
#' @return output Plotly plot object
#'
#' @export
#'
#' @examples
#' plotly_barplot(matrix(c(3, 1, 2, 4), nrow = 2, dimnames = list(c("a", "b"), c("x", "y"))))
#'
plotly_barplot <- function(plotmatrix, barmode = c("stack", "group", "overlay"), ylab = "Value", palette_name = COLORBLIND_PALETTE_NAME, title = NULL) {
  barmode <- match.arg(barmode)

  # If we're doing an overlay plot, re-order the rows so we've a better chance of seeing each group

  if (barmode == "overlay") {
    plotmatrix <- plotmatrix[order(rowMeans(plotmatrix), decreasing = TRUE), , drop = FALSE]
  }

  plotdata <- melt_matrix(plotmatrix)

  # plot_ly() otherwise silently alphabetises the x axis (via an auto-derived
  # categoryarray), discarding plotmatrix's column order - pin the axis to it
  # explicitly. Convert Var2 to character afterwards to prevent it being
  # interpreted as a numeric/continuous axis when column names look numeric.

  column_order <- levels(plotdata$Var2)
  plotdata$Var2 <- as.character(plotdata$Var2)

  palette <- makeColorScale(length(unique(plotdata$Var1)), palette = palette_name)

  plotdata %>%
    plot_ly(x = ~Var2, y = ~value, color = ~Var1, colors = palette, type = "bar") %>%
    layout(
      title = title, margin = list(b = 100), barmode = barmode, showlegend = nrow(plotmatrix) > 1,
      xaxis = list(title = " ", categoryorder = "array", categoryarray = column_order), yaxis = list(title = ylab)
    )
}

#' Tally a categorical annotation column, optionally split by a second, into a
#' \code{\link{plotly_barplot}}-ready matrix
#'
#' Rows of the returned matrix are the (optional) \code{fill} levels, columns
#' are the \code{category} levels present in \code{annotation}, ordered by
#' descending total count.
#'
#' @noRd
countMatrixByCategory <- function(annotation, category, fill = NULL) {
  category_values <- as.character(annotation[[category]])

  if (is.null(fill)) {
    counts <- table(category_values)
    plotmatrix <- matrix(as.numeric(counts), nrow = 1, dimnames = list("Count", names(counts)))
  } else {
    fill_values <- as.character(annotation[[fill]])
    plotmatrix <- as.matrix(table(fill_values, category_values))
    plotmatrix <- plotmatrix[, colSums(plotmatrix) > 0, drop = FALSE]
  }

  plotmatrix[, order(colSums(plotmatrix), decreasing = TRUE), drop = FALSE]
}

#' Plot counts of feature annotation rows by category, optionally split by a
#' second categorical column, with \code{plot_ly()}
#'
#' Tallies a categorical column from a feature annotation table (e.g.
#' \code{mcols()}/\code{rowData()} of a \code{SummarizedExperiment}, or the
#' data frame returned by the \code{selectmatrix} module's
#' \code{getAnnotation()}) and renders the counts as a bar chart via
#' \code{\link{plotly_barplot}}. Generalises the shape of the
#' differential-expression-by-biotype plot rendered by the
#' nf-core/differentialabundance report to any categorical annotation column.
#'
#' @param annotation A data frame of feature annotation, one row per feature.
#' @param category Name of a column in \code{annotation} to count rows by;
#'   forms the plot's x axis.
#' @param fill Optional name of a second column in \code{annotation} to split
#'   counts by (bar colour/legend). Default \code{NULL}: a single, unsplit
#'   count per category.
#' @param barmode Bar mode when \code{fill} is specified: \code{"group"}
#'   (dodged bars, the default) or \code{"stack"}.
#' @param palette_name Valid R color palette name
#' @param title Plot title
#'
#' @return output Plotly plot object
#'
#' @export
#'
#' @examples
#' plotly_count_barplot(
#'   data.frame(
#'     biotype = c("protein_coding", "protein_coding", "lncRNA", "lncRNA"),
#'     direction = c("Up", "Down", "Up", "Up")
#'   ),
#'   category = "biotype", fill = "direction"
#' )
#'
plotly_count_barplot <- function(annotation, category, fill = NULL, barmode = c("group", "stack"), palette_name = COLORBLIND_PALETTE_NAME, title = NULL) {
  barmode <- match.arg(barmode)

  if (!category %in% colnames(annotation)) {
    stop("plotly_count_barplot(): '", category, "' is not a column of annotation")
  }
  if (!is.null(fill) && !fill %in% colnames(annotation)) {
    stop("plotly_count_barplot(): '", fill, "' is not a column of annotation")
  }

  plotmatrix <- countMatrixByCategory(annotation, category, fill)

  if (ncol(plotmatrix) == 0) {
    stop("plotly_count_barplot(): '", category, "' has no non-missing values to count")
  }

  if (is.null(title)) {
    title <- paste("Counts by", prettifyVariablename(category))
  }

  plotly_barplot(plotmatrix, barmode = barmode, ylab = "Count", palette_name = palette_name, title = title)
}
