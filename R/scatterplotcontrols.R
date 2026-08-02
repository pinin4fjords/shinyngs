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
    axisChoices <- reactive({
      datamatrix <- getDatamatrix()
      validate(need(ncol(datamatrix) >= 2, "A scatter plot requires at least two columns"))
      labels <- colnames(datamatrix)
      if (is.null(labels)) {
        labels <- seq_len(ncol(datamatrix))
      }
      structure(seq_len(ncol(datamatrix)), names = labels)
    })

    supports3d <- reactive({
      length(axisChoices()) >= 3
    })

    requestedThreedee <- reactive({
      if (is.null(input$threedee)) default_3d else isTRUE(as.logical(input$threedee))
    })

    getThreedee <- reactive({
      requestedThreedee() && supports3d()
    })

    last3dSupport <- reactiveVal(NULL)
    observeEvent(supports3d(), {
      supported <- supports3d()
      previous <- isolate(last3dSupport())
      if (identical(supported, previous)) {
        return()
      }
      last3dSupport(supported)
      if (is.null(previous) && supported) {
        return()
      }

      if (!is.null(previous)) {
        freezeReactiveInputs(input, "threedee", "xAxis", "yAxis", "zAxis")
      }
      choices <- if (supported) c(`3D` = TRUE, `2D` = FALSE) else c(`2D` = FALSE)
      updateRadioButtons(
        session, "threedee", choices = choices,
        selected = if (supported) default_3d else FALSE, inline = TRUE
      )
    }, ignoreNULL = FALSE, priority = 1100)

    observeEvent(list(getThreedee(), axisChoices()), {
      freezeReactiveInputs(input, "xAxis", "yAxis", "zAxis")
    }, ignoreInit = TRUE, priority = 1000)

    output$plotColumns <- renderUI({
      withProgress(message = "Making scatter plot controls", value = 0, {
        ns <- session$ns

        # Work out how many axes we need

        axes <- list(x = x, y = y)
        if (getThreedee()) {
          axes$z <- z
        }

        dynamic_axes <- vapply(axes, is.na, logical(1))
        if (any(dynamic_axes)) {
          vars <- axisChoices()
        }

        # Make a select for each axis

        axis_filters <- lapply(seq_along(axes), function(n) {
          ax <- names(axes)[n]

          if (dynamic_axes[n]) {
            selectInput(ns(paste0(ax, "Axis")), paste(ax, "axis"), vars, selected = min(n, length(vars)))
          } else {
            hidden_input(ns(paste0(ax, "Axis")), axes[[n]])
          }
        })
      })
      axis_filters
    })

    # Provide accessor methods for inputs

    getAxis <- function(input_value, configured_value, default_value) {
      value <- input_value
      if (is.null(value)) {
        value <- if (is.na(configured_value)) default_value else configured_value
      }
      value <- suppressWarnings(as.numeric(value))
      available <- unname(axisChoices())
      if (length(value) != 1 || !value %in% available) {
        value <- min(default_value, length(available))
      }
      value
    }

    getXAxis <- reactive({
      getAxis(input$xAxis, x, 1)
    })

    getYAxis <- reactive({
      getAxis(input$yAxis, y, 2)
    })

    getZAxis <- reactive({
      if (getThreedee()) {
        getAxis(input$zAxis, z, 3)
      } else {
        NULL
      }
    })

    getShowLabels <- reactive({
      if (is.null(input$showLabels)) FALSE else as.logical(input$showLabels)
    })

    pointSizeValue <- reactive(input$pointSize) %>% debounce(300)

    getPointSize <- reactive({
      value <- pointSizeValue()
      req(inputsInitialised(value))
      value
    })

    inputsReady <- reactive({
      required <- list(input$threedee, input$xAxis, input$yAxis, input$showLabels, pointSizeValue())
      if (getThreedee()) {
        required <- c(required, list(input$zAxis))
      }
      if (!is.null(makeColors)) {
        required <- c(required, list(input[["scatterplot-palette_name"]]))
      }
      do.call(inputsInitialised, required)
    })

    reactives <- list(
      getXAxis = getXAxis, getYAxis = getYAxis, getZAxis = getZAxis,
      getThreedee = getThreedee, getShowLabels = getShowLabels,
      getPointSize = getPointSize, inputsReady = inputsReady
    )

    # If specified, make a palette for the specified number of colors

    if (!is.null(makeColors)) {
      reactives$getScatterPalette <- colormaker("scatterplot", getNumberCategories = makeColors)
    }

    reactives
  })
}
