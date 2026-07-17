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
    if (has_slot_data(eselist, "group_vars")) {
      selectby <- c(selectby, "group")
    }

    # We can select by sample in any case

    inputs <- list(
      h5("Select samples/ columns"), selectInput(ns("sampleSelect"), "Select samples by", selectby, selected = selectby[length(selectby)]),
      conditionalPanel(condition = paste0("input['", ns("sampleSelect"), "'] == 'name' "), checkboxGroupInput(ns("samples"), "Samples:", colnames(ese),
        selected = colnames(ese), inline = TRUE
      ))
    )

    # Add in group selection if relevant

    if (has_slot_data(eselist, "group_vars")) {
      inputs <- pushToList(inputs, conditionalPanel(
        condition = paste0("input['", ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"),
          "Define groups by:", structure(eselist@group_vars, names = prettifyVariablename(eselist@group_vars)),
          selected = defaultGroupvar(eselist)
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
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param getExperiment Reactive expression that returns a
#'   \code{ExploratorySummarizedExperiment} with assays and metadata. Usually a
#'   result of a user selection
#' @param allow_summarise Boolean, show controls for matrix summarisation?
#'
#' @return output A list of reactive functions for interrogating the selected
#' samples/ columns.
#'
#' @keywords shiny
#'
#' @examples
#' selectSamples <- sampleselect("selectmatrix", getExperiment)
#'
sampleselect <- function(id, eselist, getExperiment, allow_summarise = TRUE) {
  moduleServer(id, function(input, output, session) {
    if (allow_summarise) {
      getSummaryType <- summarisematrix("summarise")
    }

    # Render the sampleGroupVal() element based on sampleGroupVar

    output$groupSamples <- renderUI({
      ese <- getExperiment()

      if (input$sampleSelect == "group" && has_slot_data(eselist, "group_vars")) {
        validate(need(input$sampleGroupVar, FALSE))
        group_values <- as.character(unique(ese[[isolate(input$sampleGroupVar)]]))
        ns <- session$ns

        inputs <- list(checkboxGroupInput(ns("sampleGroupVal"), "Groups", group_values, selected = group_values))
        if (allow_summarise) {
          inputs <- pushToList(inputs, summarisematrixInput(ns("summarise")))
        }
        inputs
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
        validate(need(!is.null(getSampleSelect()), "Waiting for form to provide sampleSelect"))
        ese <- getExperiment()

        if (getSampleSelect() == "all") {
          return(colnames(ese))
        } else {
          validate(need(!is.null(input$samples), "Waiting for form to provide samples"))

          if (has_slot_data(eselist, "group_vars")) {
            validate(need(!is.null(input$sampleGroupVal), FALSE))
          }

          if (getSampleSelect() == "name") {
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

    reactives <- list(selectSamples = selectSamples, getSampleGroupVar = getSampleGroupVar, getSampleSelect = getSampleSelect)

    if (allow_summarise) {
      reactives[["getSummaryType"]] <- getSummaryType
    }

    reactives
  })
}
