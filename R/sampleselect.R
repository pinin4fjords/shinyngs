#' The UI input function of the sampleselect module
#'
#' This module provides controls for selecting matrix columns by sample or group
#' name.
#'
#' This will generally not be called directly, but by other modules such as the
#' selectmatrix module.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param getExperiment Reactive expression that returns a
#'   \code{ExploratorySummarizedExperiment} with assays and metadata. Usually a
#'   result of a user selection
#' @param select_samples Select samples at all? If set to false, a hidden input
#'   indicating the selection of all samples is produced.
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' sampleselectInput(ns("heatmap"))
#'
sampleselectInput <- function(id, eselist, getExperiment, select_samples = TRUE) {
  ns <- NS(id)

  ese <- getExperiment()

  if (select_samples) {
    # If grouping variables have been supplied we can use them to define sample selection

    selectby <- "name"
    if (length(eselist@group_vars) > 0) {
      selectby <- c(selectby, "group")
      default_groupvar <- ese$group_vars[1]
      if (length(eselist@group_vars) > 0) {
        default_groupvar <- eselist@default_groupvar
      }
    }

    # We can select by sample in any case

    inputs <- list(
      h5("Select samples/ columns"), selectInput(ns("sampleSelect"), "Select samples by", selectby, selected = selectby[length(selectby)]),
      conditionalPanel(condition = paste0("input['", ns("sampleSelect"), "'] == 'name' "), checkboxGroupInput(ns("samples"), "Samples:", colnames(ese),
        selected = colnames(ese), inline = TRUE
      ))
    )

    # Add in group selection if relevant

    if (length(eselist@group_vars) > 0) {
      inputs <- pushToList(inputs, conditionalPanel(
        condition = paste0("input['", ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"),
          "Define groups by:", structure(eselist@group_vars, names = prettifyVariablename(eselist@group_vars)),
          selected = eselist@default_groupvar
        ),
        uiOutput(ns("groupSamples"))
      ))
    }
  } else {
    inputs <- list(hiddenInput(ns("sampleSelect"), "all"))
  }

  tagList(inputs)
}

#' The server function of the sampleselect module
#'
#' This module provides controls for selecting matrix columns by sample or
#' group name.
#'
#' This function is not called directly, but rather via callModule() (see
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param getExperiment Reactive expression that returns a
#'   \code{ExploratorySummarizedExperiment} with assays and metadata. Usually a
#'   result of a user selection
#'
#' @return output A list of reactive functions for interrogating the selected
#' samples/ columns.
#'
#' @keywords shiny
#'
#' @examples
#' selectSamples <- callModule(sampleselect, "selectmatrix", getExperiment)
#'
sampleselect <- function(input, output, session, eselist, getExperiment) {
  getSummaryType <- callModule(summarisematrix, "summarise")

  # Render the sampleGroupVal() element based on sampleGroupVar

  output$groupSamples <- renderUI({
    ese <- getExperiment()

    if (input$sampleSelect == "group" && length(eselist@group_vars) > 0) {
      validate(need(input$sampleGroupVar, FALSE))
      group_values <- as.character(unique(ese[[isolate(input$sampleGroupVar)]]))
      ns <- session$ns

      list(checkboxGroupInput(ns("sampleGroupVal"), "Groups", group_values, selected = group_values), summarisematrixInput(ns("summarise")))
    }
  })

  # Output a reactive so that other modules know whether we've selected by sample or group

  getSampleSelect <- reactive({
    input$sampleSelect
  })

  # Return summary type

  getSampleGroupVar <- reactive({
    input$sampleGroupVar
  })

  # Reactive expression for selecting the specified columns

  selectSamples <- reactive({
    withProgress(message = "Selecting samples", value = 0, {
      ese <- getExperiment()

      validate(need(!is.null(input$sampleSelect), "Waiting for form to provide sampleSelect"))

      if (input$sampleSelect == "all") {
        return(colnames(ese))
      } else {
        validate(need(!is.null(input$samples), "Waiting for form to provide samples"))

        if (length(eselist@group_vars) > 0) {
          validate(need(!is.null(input$sampleGroupVal), FALSE))
        }

        if (input$sampleSelect == "name") {
          return(input$samples)
        } else {
          # Any NA in the colData will become string '' via the inputs, so make sure we consider that when matching

          samplegroups <- as.character(ese[[isolate(input$sampleGroupVar)]])
          samplegroups[is.na(samplegroups)] <- ""

          return(colnames(ese)[samplegroups %in% input$sampleGroupVal])
        }
      }
    })
  })

  list(selectSamples = selectSamples, getSampleGroupVar = getSampleGroupVar, getSummaryType = getSummaryType, getSampleSelect = getSampleSelect)
}
