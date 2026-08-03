#' The UI function of the groupby module
#'
#' The groupby module provides a UI element to choose from the
#' \code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA etc
#'
#' @param id Submodule namespace
#' @param color Provide coloring functionality for groups?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' groupbyInput(ns("heatmap"))
#'
groupbyInput <- function(id, color = TRUE) {
  ns <- NS(id)

  fields <- list(uiOutput(ns("groupby_fields")))

  if (color) {
    fields <- push_to_list(fields, colormakerInput(ns("groupby")))
  }

  fields
}

#' The server function of the groupby module
#'
#' The groupby module provides a UI element to choose from the
#' \code{group_vars} in a SummarizedExperment. Useful for coloring in a PCA etc
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param group_label A label for the grouping field
#' @param multiple Produces a checkbox group if true, a select box if false
#' @param selectColData Reactive returning an experiment matrix, probably
#'   derived from the \code{\link{selectmatrix}} module.
#' @param isDynamic Reactive expression providing a boolean. A FALSE value
#'   causes the groupby option to be placed in a hidden field.
#' @param color Require and return palette controls?
#'
#' @return output A list of reactive functions which will be used by other
#' modules.
#'
#' @keywords shiny
#'
#' @examples
#' geneset_functions <- groupby("heatmap", getExperiment)
#'
groupby <- function(id, eselist, group_label = "Group by", multiple = FALSE, selectColData = NULL, isDynamic = reactive({
                      TRUE
                    }), color = TRUE) {
  moduleServer(id, function(input, output, session) {
    if (color) {
      getPalette <- colormaker("groupby", getNumberCategories = getNumberCategories)
    } else {
      getPalette <- reactive(NULL)
    }

    # Choose a default grouping variable, either the one specified or the first

    getDefaultGroupby <- reactive({
      if (multiple) {
        eselist@group_vars
      } else {
        if (has_slot_data(eselist, "default_groupvar")) {
          eselist@default_groupvar
        } else {
          eselist@group_vars[1]
        }
      }
    })

    groupbyUiContext <- reactive({
      list(
        dynamic = isDynamic(),
        default = if (has_slot_data(eselist, "group_vars")) getDefaultGroupby() else "NULL"
      )
    })

    observeEvent(groupbyUiContext(), {
      freezeReactiveInputs(input, "groupby")
    }, ignoreInit = TRUE, priority = 1000)

    # Render function for the field

    output$groupby_fields <- renderUI({
      withProgress(message = "Rendering group by", value = 0, {
        ns <- session$ns

        if (has_slot_data(eselist, "group_vars")) {
          dynamic <- isDynamic()

          group_options <- structure(eselist@group_vars, names = prettify_variable_name(eselist@group_vars))

          if (multiple) {
            groupinput <- checkboxGroupInput(ns("groupby"), group_label, group_options, selected = group_options, inline = TRUE)
          } else {
            groupinput <- selectInput(ns("groupby"), group_label, group_options, selected = getDefaultGroupby())
          }

          if (!dynamic) {
            groupinput <- shinyjs::hidden(groupinput)
          }

          groupinput
        } else {
          hidden_input(ns("groupby"), "NULL")
        }
      })
    })

    # Return a reactive that retrieves the field value

    getGroupby <- reactive({
      selected <- input$groupby
      if (is.null(selected)) {
        if (!has_slot_data(eselist, "group_vars")) {
          return(NULL)
        }
        selected <- getDefaultGroupby()
      }
      validate(need(length(selected) > 0, "Select at least one grouping variable"))
      if (selected[1] == "NULL") {
        NULL
      } else {
        selected
      }
    })

    # Get the number of categories given an input experiment matrixa and the selected grouping variable.

    getNumberCategories <- reactive({
      group_by <- getGroupby()
      coldata <- selectColData()

      if (!is.null(group_by) && !is.null(coldata)) {
        length(unique(coldata[[group_by]]))
      }
    })

    inputsReady <- reactive({
      required <- list(input$groupby)
      if (color) {
        required <- c(required, list(input[["groupby-palette_name"]]))
      }
      if (!do.call(inputsInitialised, required)) {
        return(FALSE)
      }
      if (!has_slot_data(eselist, "group_vars")) {
        return(identical(input$groupby, "NULL"))
      }
      all(input$groupby %in% eselist@group_vars)
    })

    list(
      getGroupby = getGroupby, getNumberCategories = getNumberCategories,
      getPalette = getPalette, inputsReady = inputsReady
    )
  })
}
