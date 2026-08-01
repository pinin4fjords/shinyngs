#' Input function for scatterplotcontrols module
#'
#' This module provides controls (2D/3D, axes etc) for scatter plots, which
#' may then be used by one or more instances of the scatterplot module.
#'
#' @param id Submodule namespace
#' @param allow_3d Boolean: allow user to choose 3D plotting?
#' @param make_colors Boolean: add controls for coloring?
#' @param default_3d Boolean: select 3D when the controls first render?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @examples
#' scatterplotcontrolsInput("pca", allow_3d = FALSE) # for a 2D plot
#'
scatterplotcontrolsInput <- function(id, allow_3d = TRUE, make_colors = FALSE, default_3d = TRUE) {
  ns <- NS(id)

  if (allow_3d) {
    inputs <- list(radioButtons(ns("threedee"), "Plot type", c(`3D` = TRUE, `2D` = FALSE), selected = default_3d, inline = TRUE))
  } else {
    inputs <- list(hidden_input(ns("threedee"), FALSE))
  }

  if (make_colors) {
    inputs <- c(inputs, list(colormakerInput(ns("scatterplot"))))
  }

  c(inputs, list(uiOutput(ns("plotColumns")), checkboxInput(ns("showLabels"), "Show labels?"), sliderInput(ns("pointSize"), "Point size",
    min = 1, max = 20,
    value = 5
  )))
}

#' Server function for scatterplotcontrols module
#'
#' This module provides controls (2D/3D, axes etc) for scatter plots, which
#' may then be used by one or more instances of the scatterplot module.
#'
#' @param id Module namespace
#' @param getDatamatrix Reactive expression that returns a matrix from which
#' coulumn headers will be used to create axis select drop-downs. The same
#' reactive should be supplied to the scatterplot module
#' @param x A value supplied for this parameter will cause a hidden field to
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#' @param y A value supplied for this parameter will cause a hidden field to
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#' @param z A value supplied for this parameter will cause a hidden field to
#' be generated instead of a select, useful for scatter plots that don't need
#' the user to select axes (default: NA)
#' @param makeColors Boolean: use controls for coloring?
#' @param default_3d Boolean: use 3D until the matching browser input is ready?
#'
#' @return output A list of reactives for accessing input values
#'
#' @examples
#' scatterplotcontrols_reactives <- scatterplotcontrols("pca", pcaMatrix, x = 1, y = 2) # To have fixed axes rather than user-selected
#'
scatterplotcontrols <- function(id, getDatamatrix, x = NA, y = NA, z = NA, makeColors = NULL, default_3d = TRUE) {
  moduleServer(id, function(input, output, session) {
    output$plotColumns <- renderUI({
      withProgress(message = "Making scatter plot controls", value = 0, {
        ns <- session$ns
        datamatrix <- getDatamatrix()
        vars <- structure(seq_len(ncol(datamatrix)), names = colnames(datamatrix))

        # Work out how many axes we need

        axes <- list(x = x, y = y)
        if (getThreedee()) {
          axes$z <- z
        }

        # Make a select for each axis

        axis_filters <- lapply(seq_along(axes), function(n) {
          ax <- names(axes)[n]

          if (is.na(axes[n])) {
            selectInput(ns(paste0(ax, "Axis")), paste(ax, "axis"), vars, selected = n)
          } else {
            hidden_input(ns(paste0(ax, "Axis")), axes[n])
          }
        })
      })
      axis_filters
    })

    # Provide accessor methods for inputs

    getXAxis <- reactive({
      value <- input$xAxis
      if (is.null(value)) value <- if (is.na(x)) 1 else x
      as.numeric(value)
    })

    getYAxis <- reactive({
      value <- input$yAxis
      if (is.null(value)) value <- if (is.na(y)) 2 else y
      as.numeric(value)
    })

    getZAxis <- reactive({
      if (getThreedee()) {
        value <- input$zAxis
        if (is.null(value)) value <- if (is.na(z)) 3 else z
        as.numeric(value)
      } else {
        NULL
      }
    })

    getThreedee <- reactive({
      if (is.null(input$threedee)) default_3d else as.logical(input$threedee)
    })

    getShowLabels <- reactive({
      if (is.null(input$showLabels)) FALSE else as.logical(input$showLabels)
    })

    getPointSize <- reactive({
      if (is.null(input$pointSize)) 5 else input$pointSize
    }) %>% debounce(300)

    reactives <- list(getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis, getThreedee = getThreedee, getShowLabels = getShowLabels, getPointSize = getPointSize)

    # If specified, make a palette for the specified number of colors

    if (!is.null(makeColors)) {
      reactives$getScatterPalette <- colormaker("scatterplot", getNumberCategories = makeColors)
    }

    reactives
  })
}
