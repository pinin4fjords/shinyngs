#' The input function of the \code{labelselectfield} module
#'
#' This module provides an input which allows filtering on the basis of data in
#' the metadata slot of an \code{ExploratorySummarizedExperiment}. It will only
#' be called by other modules requiring that input.
#'
#' Where metadata is present, the user can select which field to select on, and
#' the value of that field (populated conditionall on field selction). If
#' specified, checkboxes are provided to allow selection of specific row IDs.
#'
#' A \code{\link[shiny]{selectizeInput}} is used for performance reasons,
#' providing an autocomplete field for selecting from a list that could stretch
#' to thousands of entries. This would be difficult to do client-side using a
#' standard select field.
#'
#' @param id Submodule namespace
#' @param max_items Maximum number of items that can be selected
#' @param id_selection Allow users to pick specific ID from those that relate
#'   to the specified label? (default: FALSE)
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @examples
#' labelselectfieldInput("myid")
#'
labelselectfieldInput <- function(id, max_items = 1, id_selection = FALSE) {
  ns <- NS(id)

  filters <- list(uiOutput(ns("metaFields")), uiOutput(ns("metaValue")))

  if (id_selection) {
    filters <- push_to_list(filters, uiOutput(ns("labelIds")))
  }

  filters
}

#' The server function of the \code{labelselectfield} module
#'
#' This module provides an input which allows filtering on the basis of data in
#' the metadata slot of an \code{ExploratorySummarizedExperiment}. It will only
#' be called by other modules requiring that input.
#'
#' Where metadata is present, the user can select which field to select on, and
#' the value of that field (populated conditionall on field selction). If
#' specified, checkboxes are provided to allow selection of specific row IDs.
#'
#' A \code{\link[shiny]{selectizeInput}} is used for performance reasons,
#' providing an autocomplete field for selecting from a list that could stretch
#' to thousands of entries. This would be difficult to do client-side using a
#' standard select field.
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param getExperiment Reactive supplying
#'   an \code{ExploratorySummarizedExperiment}
#' @param labels_from_all_experiments Derive valid labels from all experiments?
#' @param url_field Parameter to extract from the URL to set the value of the
#'   select field
#' @param max_items Maximum number of metadata values that can be selected
#' @param field_selection Allow selection of the meta field to use (TRUE), or
#'   use labels (or row ids if the label field is not set)?
#' @param id_selection Allow users to pick specific ID from those that relate
#'   to the specified label? (default: FALSE)
#' @param getNonEmptyRows Optional reactive providing non empty matrix rows
#' @param list_input Boolean: will input be a list of values?
#'
#' @examples
#' labelselectfield("myid", eselist)
#'
labelselectfield <- function(id, eselist, getExperiment = NULL, labels_from_all_experiments = FALSE, url_field = "label", max_items = 1, field_selection = FALSE, id_selection = FALSE, getNonEmptyRows = NULL, list_input = FALSE) {
  moduleServer(id, function(input, output, session) {
    input_generation <- new.env(parent = emptyenv())
    input_generation$experiments <- NULL
    input_generation$meta_field <- NULL
    input_generation$label <- NULL

    # This module will normally be initialised with a reactive that returns the currently selected experiment, whose metadata will be used for gene symbols
    # etc.  But if that reactive is not present, we can use values from ALL experiments. In the latter case the field will be more static, in the former it
    # will depend on the value of any experiment-selecting field.

    # A wrapper reactive to determine the experiments to consider when parsing metadata. Use all experiments if a reactie is not supplied.

    getExperiments <- reactive({
      if (is.null(getExperiment)) {
        eselist
      } else {
        list(getExperiment())
      }
    })

    ### META-FIELD SELECTION

    # Create an input for picking which metafield to use. Allow the choice of any non-numeric field

    getMetaFields <- reactive({
      ese <- getExperiment()
      names(mcols(ese))[!unlist(lapply(mcols(ese), is.numeric))]
    })

    getDefaultMetaField <- reactive({
      ese <- getExperiment()
      if (length(ese@labelfield) > 0) {
        return(ese@labelfield)
      }
      if (has_slot_data(ese, "idfield")) ese@idfield else "id"
    })

    experimentInputContext <- reactive({
      lapply(getExperiments(), function(ese) {
        list(
          rows = rownames(ese),
          id_field = if (has_slot_data(ese, "idfield")) ese@idfield else "id",
          label_field = ese@labelfield
        )
      })
    })

    observeEvent(experimentInputContext(), {
      context <- experimentInputContext()
      if (!is.null(input_generation$experiments) && !identical(context, input_generation$experiments)) {
        freezeReactiveInputs(input, "metaField", "label", "ids")
      }
      input_generation$experiments <- context
    }, priority = 1000)

    observeEvent(input$metaField, {
      meta_field <- input$metaField
      if (!is.null(input_generation$meta_field) && !identical(meta_field, input_generation$meta_field)) {
        freezeReactiveInputs(input, "label", "ids")
      }
      input_generation$meta_field <- meta_field
    }, ignoreNULL = TRUE, priority = 1000)

    observeEvent(input$label, {
      label <- input$label
      if (!is.null(input_generation$label) && !identical(label, input_generation$label)) {
        freezeReactiveInputs(input, "ids")
      }
      input_generation$label <- label
    }, ignoreNULL = TRUE, priority = 1000)

    output$metaFields <- renderUI({
      ns <- session$ns

      ese <- getExperiment()

      if (field_selection) {
        metaFields <- getMetaFields()
        selectInput(ns("metaField"), label = "Metadata field", choices = structure(metaFields, names = prettify_variable_name(metaFields)), selected = getDefaultMetaField())
      } else {
        hidden_input(ns("metaField"), values = getDefaultMetaField())
      }
    })

    # Fetch the meta field from the input

    getSelectedMetaField <- reactive({
      valid_fields <- if (field_selection) getMetaFields() else getDefaultMetaField()
      selected <- input$metaField
      if (length(selected) != 1 || !selected %in% valid_fields) getDefaultMetaField() else selected
    })

    ### META-FIELD VALUE SELECTION

    # Set the meta values filter dependent on field

    output$metaValue <- renderUI({
      ns <- session$ns
      mf <- getSelectedMetaField()

      if (list_input) {
        tags$textarea(id = ns("label"), rows = 3, cols = 20, "Paste list here, one per line")
      } else {
        selected <- restored_label
        if (is.null(selected)) {
          selected <- getDefaultLabel()
        }
        selectizeInput(ns("label"), prettify_variable_name(mf), choices = selected, selected = selected, options = list(
          placeholder = "Type a value or scroll", maxItems = max_items,
          addPrecedence = TRUE
        ))
      }
    })

    # A server-side selectize holds no options client-side, so a bookmarked
    # input$label can't restore itself. Capture the value from the bookmark and
    # re-apply it as the selection once choices are populated below.

    restored_label <- NULL

    onRestore(function(state) {
      val <- bookmarkedInputValue(state, session, "label")
      if (!is.null(val)) {
        restored_label <<- val
      }
    })

    getDefaultLabel <- reactive({
      labels <- getValidLabels()
      if (length(labels) == 0 || is.null(getNonEmptyRows)) {
        return(labels[1])
      }

      ese <- getExperiment()
      ids <- intersect(getNonEmptyRows(), rownames(ese))
      mf <- getSelectedMetaField()
      current_labels <- if (mf == "id" || mf == ese@idfield) {
        ids
      } else {
        as.character(mcols(ese)[[mf]][match(ids, rownames(ese))])
      }
      current_labels <- current_labels[!is.na(current_labels) & nzchar(current_labels)]
      candidates <- labels[labels %in% current_labels]
      if (length(candidates) > 0) candidates[1] else labels[1]
    })

    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list

    observeEvent(input$metaField, {
      if (!list_input) {
        selected <- restored_label
        restored_label <<- NULL
        if (is.null(selected)) {
          selected <- getDefaultLabel()
        }
        updateSelectizeInput(session, "label", choices = getValidLabels(), selected = selected, server = TRUE)
      }
    })

    # Get the list of labels. This will be used to populate the autocomplete field

    getValidLabels <- reactive({
      if (labels_from_all_experiments) {
        exps <- eselist
      } else {
        exps <- getExperiments()
      }

      label_lists <- lapply(exps, function(ese) {
        mf <- getSelectedMetaField()
        if (mf == "id" || mf == ese@idfield) {
          rownames(ese)
        } else {
          mcols(ese)[[mf]]
        }
      })

      labels <- unique(Reduce(union, label_lists))
      labels[order(tolower(labels))]
    })

    # Get the value of the label field

    getSelectedLabels <- reactive({
      # mf <- getSelectedMetaField()
      validate(need(!is.null(input$label) && input$label != "", FALSE))

      if (list_input) {
        unlist(strsplit(input$label, "\\n"))
      } else {
        input$label
      }
    })

    ### SELECTION OF IDS FOR METAFIELD VALUES

    # Allow selection from the ids pertaining to a given label

    output$labelIds <- renderUI({
      ns <- session$ns

      ids <- getAssociatedIds()

      if (length(ids) == 1) {
        hidden_input(ns("ids"), ids)
      } else {
        checkboxGroupInput(ns("ids"), label = "Associated IDs", choices = getAssociatedIds(), selected = getAssociatedIds())
      }
    })

    # Get the row or rows of the data that correspond to the input metadata

    getSelectedIds <- reactive({
      # If the user has been allowed to select IDs, fetch the value of the input field. Othewise return all IDs associated with the selected label

      if (id_selection) {
        if (is.null(input$ids)) {
          return(getAssociatedIds())
        }
        validate(need(length(input$ids) > 0, "Select at least one associated ID"))
        input$ids
      } else {
        getAssociatedIds()
      }
    })

    # Get the IDs for the selected labels

    getAssociatedIds <- reactive({
      labels <- getSelectedLabels()
      exps <- getExperiments()
      mf <- getSelectedMetaField()

      # If we're just selecting based on row ID, then we don't need to consult the metadata

      if (mf == "id") {
        labels
      } else {
        id_lists <- lapply(exps, function(ese) {
          if (mf == ese@idfield) {
            labels
          } else {
            ids <- rownames(ese)[which(mcols(ese)[[mf]] %in% labels)]

            # If an assay is specified, limit to valid IDs for that assay

            if (!is.null(getNonEmptyRows)) {
              ids <- intersect(ids, getNonEmptyRows())
            }
            ids
          }
        })
        all_ids <- sort(Reduce(union, id_lists))

        validate(need(length(all_ids) > 0, "No results for specified features. Check matrix selection"))
        all_ids
      }
    })

    # A reactive for updating the label input field

    updateLabelField <- reactive({
      query <- parseQueryString(session$clientData$url_search)
      selected <- query[[url_field]]
      if (!is.null(selected) && !identical(selected, isolate(input$label))) {
        freezeReactiveInputs(input, "label", "ids")
      }
      updateSelectizeInput(session, "label", selected = selected, choices = getValidLabels(), server = TRUE)
    })

    inputsReady <- reactive({
      valid_fields <- if (field_selection) getMetaFields() else getDefaultMetaField()
      if (!inputsInitialised(input$metaField, input$label) ||
          length(input$metaField) != 1 || !input$metaField %in% valid_fields) {
        return(FALSE)
      }
      if (!list_input && !all(input$label %in% getValidLabels())) {
        return(FALSE)
      }
      if (id_selection) {
        if (!inputsInitialised(input$ids) || !all(input$ids %in% getAssociatedIds())) {
          return(FALSE)
        }
      }
      TRUE
    })

    list(
      getSelectedLabels = getSelectedLabels, getValidLabels = getValidLabels,
      getSelectedIds = getSelectedIds, updateLabelField = updateLabelField,
      inputsReady = inputsReady
    )
  })
}
