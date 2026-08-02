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
      inputs <- push_to_list(inputs, conditionalPanel(
        condition = paste0("input['", ns("sampleSelect"), "'] == 'group' "), selectInput(ns("sampleGroupVar"),
          "Define groups by:", structure(eselist@group_vars, names = prettify_variable_name(eselist@group_vars)),
          selected = defaultGroupvar(eselist)
        ),
        uiOutput(ns("groupSamples"))
      ))
    }
  } else {
    inputs <- list(hidden_input(ns("sampleSelect"), "all"))
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
#' @param select_samples Match the UI's sample-selection setting. When false,
#'   all samples are selected.
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
sampleselect <- function(id, eselist, getExperiment, select_samples = TRUE, allow_summarise = TRUE) {
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
          inputs <- push_to_list(inputs, summarisematrixInput(ns("summarise")))
        }
        inputs
      }
    })

    # Output a reactive so that other modules know whether we've selected by sample or group

    getSampleSelect <- reactive({
      if (!is.null(input$sampleSelect)) {
        return(input$sampleSelect)
      }
      if (!select_samples) {
        return("all")
      }
      if (has_slot_data(eselist, "group_vars")) "group" else "name"
    })

    # Return summary type

    getSampleGroupVar <- reactive({
      if (is.null(input$sampleGroupVar)) defaultGroupvar(eselist) else input$sampleGroupVar
    })

    # Reactive expression for selecting the specified columns

    selectSamples <- reactive({
      withProgress(message = "Selecting samples", value = 0, {
        ese <- getExperiment()
        sample_select <- getSampleSelect()

        if (sample_select == "all") {
          return(colnames(ese))
        }

        if (sample_select == "name") {
          if (is.null(input$samples)) return(colnames(ese))
          return(input$samples)
        }

        validate(need(sample_select == "group", paste0("Unknown sample selection mode: ", sample_select)))
        validate(need(has_slot_data(eselist, "group_vars"), "No sample grouping variables are available"))
        sample_group_var <- getSampleGroupVar()
        validate(need(sample_group_var %in% colnames(SummarizedExperiment::colData(ese)), "Select a valid sample grouping variable"))

        # Any NA in the colData will become string '' via the inputs, so make sure we consider that when matching

        samplegroups <- as.character(ese[[sample_group_var]])
        samplegroups[is.na(samplegroups)] <- ""
        selected_groups <- input$sampleGroupVal
        if (is.null(selected_groups)) {
          selected_groups <- unique(samplegroups)
        }

        colnames(ese)[samplegroups %in% selected_groups]
      })
    })

    inputsReady <- reactive({
      if (!inputsInitialised(input$sampleSelect)) {
        return(FALSE)
      }
      if (input$sampleSelect == "name") {
        return(inputsInitialised(input$samples))
      }
      if (input$sampleSelect == "group") {
        required <- list(input$sampleGroupVar, input$sampleGroupVal)
        if (allow_summarise) {
          required <- c(required, list(input[["summarise-summaryType"]]))
        }
        return(do.call(inputsInitialised, required))
      }
      TRUE
    })

    reactives <- list(
      selectSamples = selectSamples, getSampleGroupVar = getSampleGroupVar,
      getSampleSelect = getSampleSelect, inputsReady = inputsReady
    )

    if (allow_summarise) {
      reactives[["getSummaryType"]] <- getSummaryType
    }

    reactives
  })
}
