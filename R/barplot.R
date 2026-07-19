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
    # Render the plot

    output$barPlot <- renderPlotly({
      validate(need(input$barMode, "Waiting for bar mode"))

      plotly_barchart(getPlotmatrix(), barmode = input$barMode, ylab = getYLabel()) %>%
        shinyngsPlotlyConfig("barplot", format = session$userData$plotFormat())
    })
  })
}

#' Make a grouped, stacked or overlaid bar chart with \code{plot_ly()}
#'
#' @param matrix A matrix to plot, e.g. counts by category (row) and sample
#'   (column)
#' @param barmode 'stack', 'group' or 'overlay'. For 'overlay', rows are
#'   reordered by decreasing mean so each is more likely to be visible
#' @param ylab Y axis label
#'
#' @return output A plotly htmlwidget
#'
#' @export
#'
#' @examples
#' m <- matrix(1:6, nrow = 2, dimnames = list(c("up", "down"), c("s1", "s2", "s3")))
#' plotly_barchart(m, barmode = "stack", ylab = "Count")
#'
plotly_barchart <- function(matrix, barmode = c("stack", "group", "overlay"), ylab = "") {
  barmode <- match.arg(barmode)

  if (barmode == "overlay") {
    matrix <- matrix[order(rowMeans(matrix), decreasing = TRUE), , drop = FALSE]
  }

  plotdata <- melt_matrix(matrix)

  # Prevent interpretation of row names as numbers

  plotdata$Var2 <- as.character(plotdata$Var2)

  plotdata %>%
    plot_ly(x = ~Var2, y = ~value, color = ~Var1, type = "bar") %>%
    layout(
      margin = list(b = 100), barmode = barmode, xaxis = list(title = " "),
      yaxis = list(title = ylab)
    )
}
