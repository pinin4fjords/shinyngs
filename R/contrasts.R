#' The input function of the contrasts module
#'
#' This module provides the form elements to control contrasts used in e.g.
#' differential expression panels. In particular it provides the ability for
#' users to add filters to progressively refine a query.
#'
#' @param id Submodule namespace
#' @param allow_filtering Provide the filtering fields? Can be disabled to
#' produce unfiltered contrasts tables.
#' @param summarise Provide summarisation controls? Allow user to control how
#'   how values are summarised per group. Disabling this disables
#'   summarisation, which may be the desired result for modules that just need
#'   to use the contrasts drop-down.
#' @param dynamic_filters Logical indicating whether the user should be able
#'   to add progressive filters.
#' @param select_summary_type Allow user to select summary type (e.g. mean)?
#'   Passed to \code{\link{summarisematrixInput}}.
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()
#'
#' @keywords shiny
#'
#' @examples
#' contrastsInput("test")
#'
contrastsInput <- function(id, allow_filtering = TRUE, summarise = TRUE, dynamic_filters = FALSE, select_summary_type = FALSE) {
  ns <- NS(id)

  inputs <- list()

  if (allow_filtering) {
    inputs <- pushToList(inputs, checkboxInput(ns("filterRows"), "Filter rows", TRUE))
  } else {
    inputs <- pushToList(inputs, shinyjs::hidden(checkboxInput(ns("filterRows"), "Filter rows", FALSE)))
  }

  # Contrasts filters added by the observeEvent() in the server function. If no dynamic filters are to be provided, then all we need to do is provide a
  # placeholder and a single set of filters will be provided when the page loads. If dynamic filters ARE to be provided, then some other buttons etc are
  # required.

  contrast_filters <- list(tags$div(id = ns("contrasts-placeholder")))

  if (dynamic_filters) {
    contrast_filters <- c(
      list(hiddenInput(ns("dynamic"), 1)), list(helpText("Build up a complex query by adding filters below"), hr()), contrast_filters,
      list(hr(), uiOutput(ns("combine_operator_ui")), actionButton(ns("insertBtn"), "Add"), HTML("&nbsp;"), actionButton(ns("removeBtn"), "Remove"))
    )
  } else {
    contrast_filters <- pushToList(contrast_filters, uiOutput(ns("combine_operator_ui")))
  }

  # inputs <- pushToList(inputs, conditionalPanel(condition = paste0('input['', ns('filterRows'), ''] == true '), contrast_filters))
  inputs <- pushToList(inputs, contrast_filters)

  if (summarise) {
    inputs <- pushToList(inputs, summarisematrixInput(ns("contrasts"), allow_none = FALSE, select_summary_type = select_summary_type))
  }

  inputs
}

#' The output function of the contrasts module
#'
#' This module provides the form elements to control contrasts used in e.g.
#' differential expression panels. In particular it provides the ability for
#' users to add filters to progressively refine a query.
#'
#' This function provides a summary. Actual output should be rendered
#' by calling modules using the provided reactives.
#'
#' @param id Submodule namespace
#'
#' @examples
#' contrastsOutput("myid")
#'
contrastsOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary"))
}

#' The server function of the contrasts module
#'
#' This module provides the form elements to control contrasts used in e.g.
#' differential expression panels. In particular it provides the ability for
#' users to add filters to progressively refine a query.
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' The reactive graph is built by a sequence of internal factory functions,
#' each owning one cohesive stage (contrast enumeration, naming, the dynamic
#' filter-set engine, table building, selection, filtering, labelling, and
#' the query summary) - see \code{contrastEnumeration}, \code{contrastNaming},
#' \code{contrastFilterSetEngine}, \code{contrastTableBuilder},
#' \code{contrastSelection}, \code{contrastFiltering}, \code{contrastLabelling}
#' and \code{contrastQuerySummary}. This function itself just wires those
#' stages together in dependency order and returns their public reactives.
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param selectmatrix_reactives The list of reactive expressions returned by
#'   a call to the \code{\link{selectmatrix}} module. This will be unpacked to
#'   gain access to the data provided by those reaactive.
#' @param multiple Allow selection of multiple contrasts?
#' @param select_all_contrasts Select all contrasts by default?
#' @param show_controls Show the controls for contrast selection?
#' @param default_foldchange default value for the fold change filter
#' @param default_pval Default value for the p value field
#' @param default_qval Default value for the q value field
#'
#' @keywords shiny
#'
#' @examples
#' contrasts("differential", eselist = eselist, selectmatrix_reactives = selectmatrix_reactives, multiple = TRUE)
#'
contrasts <- function(id, eselist, selectmatrix_reactives = list(), multiple = FALSE, select_all_contrasts = FALSE, show_controls = TRUE, default_foldchange = 2, default_pval = 0.05, default_qval = 0.1) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    enumeration <- contrastEnumeration(eselist, selectmatrix_reactives)
    naming <- contrastNaming(enumeration$getAllContrasts)

    filter_set <- contrastFilterSetEngine(
      ns, input, output, session, selectmatrix_reactives,
      multiple = multiple, show_controls = show_controls,
      default_foldchange = default_foldchange, default_pval = default_pval, default_qval = default_qval,
      select_all_contrasts = select_all_contrasts,
      getAllContrasts = enumeration$getAllContrasts,
      getAllContrastsNumbers = enumeration$getAllContrastsNumbers,
      fcsAvailable = enumeration$fcsAvailable,
      pvalsAvailable = enumeration$pvalsAvailable,
      qvalsAvailable = enumeration$qvalsAvailable
    )

    tables <- contrastTableBuilder(
      selectmatrix_reactives,
      getSummaries = enumeration$getSummaries,
      getAllContrasts = enumeration$getAllContrasts,
      getAllContrastsNumbers = enumeration$getAllContrastsNumbers,
      fcsAvailable = enumeration$fcsAvailable,
      pvalsAvailable = enumeration$pvalsAvailable,
      qvalsAvailable = enumeration$qvalsAvailable
    )

    selection <- contrastSelection(
      getSelectedContrastNumbers = filter_set$getSelectedContrastNumbers,
      getAllContrasts = enumeration$getAllContrasts,
      getContrastSamples = enumeration$getContrastSamples,
      makeContrastNames = naming$makeContrastNames,
      makeSafeContrastNames = naming$makeSafeContrastNames
    )

    filtering <- contrastFiltering(
      selectmatrix_reactives,
      getSelectedContrastNumbers = filter_set$getSelectedContrastNumbers,
      contrastsTablesToMatchMatrix = tables$contrastsTablesToMatchMatrix,
      singleContrast = selection$singleContrast,
      getFoldChange = filter_set$getFoldChange,
      getFoldChangeCard = filter_set$getFoldChangeCard,
      getPval = filter_set$getPval,
      getPvalCard = filter_set$getPvalCard,
      getQval = filter_set$getQval,
      getQvalCard = filter_set$getQvalCard,
      pvalsAvailable = enumeration$pvalsAvailable,
      qvalsAvailable = enumeration$qvalsAvailable,
      getFilterRows = filter_set$getFilterRows,
      getFilterSetCombinationOperator = filter_set$getFilterSetCombinationOperator
    )

    labelling <- contrastLabelling(
      eselist, selectmatrix_reactives,
      selectFinalFeatures = filtering$selectFinalFeatures,
      filteredContrastsTables = filtering$filteredContrastsTables
    )

    query_summary <- contrastQuerySummary(
      output, selectmatrix_reactives,
      filteredContrastsTables = filtering$filteredContrastsTables,
      getSelectedContrasts = selection$getSelectedContrasts,
      makeContrastNames = naming$makeContrastNames,
      getFilterSetValues = filter_set$getFilterSetValues,
      selectFilterFinalFeatures = filtering$selectFilterFinalFeatures,
      selectFinalFeatures = filtering$selectFinalFeatures,
      getFilterSetCombinationOperator = filter_set$getFilterSetCombinationOperator
    )

    list(
      getFoldChange = filter_set$getFoldChange, getFoldChangeCard = filter_set$getFoldChangeCard, getFoldChangeScale = enumeration$getFoldChangeScale,
      getQval = filter_set$getQval, getQvalCard = filter_set$getQvalCard, getPval = filter_set$getPval, getPvalCard = filter_set$getPvalCard,
      getAllContrasts = enumeration$getAllContrasts, getSelectedContrasts = selection$getSelectedContrasts, getSelectedContrastNumbers = filter_set$getSelectedContrastNumbers,
      getSelectedContrastNames = selection$getSelectedContrastNames, getSafeSelectedContrastNames = selection$getSafeSelectedContrastNames,
      getContrastSamples = enumeration$getContrastSamples, getSelectedContrastSamples = selection$getSelectedContrastSamples,
      contrastsTables = tables$contrastsTables, filteredContrastsTables = filtering$filteredContrastsTables, labelledContrastsTable = labelling$labelledContrastsTable,
      linkedLabelledContrastsTable = labelling$linkedLabelledContrastsTable,
      makeDifferentialSetSummary = query_summary$makeDifferentialSetSummary, getQueryStrings = query_summary$getQueryStrings, selectedContrastsTables = filtering$selectedContrastsTables
    )
  })
}

#' Enumerate contrasts and their per-group summary statistics
#'
#' Owns the reactives that read the raw \code{contrasts} slot and, where
#' requested, calculate per-contrast-variable group summaries (means etc) -
#' the data every later stage subsets and filters. Independent of the
#' dynamic filter-set UI: it only reads \code{eselist}/\code{selectmatrix_reactives}.
#'
#' @param eselist ExploratorySummarizedExperimentList object
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#'
#' @return A list of reactives: \code{getSummaries}, \code{getAllContrasts},
#'   \code{getAllContrastsNumbers}, \code{getContrastSamples},
#'   \code{fcsAvailable}, \code{getFoldChangeScale}, \code{pvalsAvailable},
#'   \code{qvalsAvailable}
#' @noRd
contrastEnumeration <- function(eselist, selectmatrix_reactives) {
  getSummaryType <- summarisematrix("contrasts")

  # Generate the summary statistic (probably mean) for column groups as defined by the possible contrasts. Other functions can then pick from this output and
  # calculate fold changes etc.

  getSummaries <- reactive({
    if (!is.null(getSummaryType())) {
      ese <- selectmatrix_reactives$getExperiment()
      contrasts <- getAllContrasts()
      matrix <- selectmatrix_reactives$getAssayMatrix()
      coldata <- data.frame(colData(ese), check.names = FALSE)

      validate(need(nrow(matrix) > 0, "Waiting for input matrix"))

      contrast_variables <- unique(unlist(lapply(contrasts, function(x) x["Variable"])))
      names(contrast_variables) <- contrast_variables

      withProgress(message = paste("Calculating summaries by", getSummaryType()), value = 0, {
        summaries <- lapply(contrast_variables, function(cv) summarizeMatrix(matrix, coldata[[cv]], getSummaryType()))
      })

      summaries
    }
  })

  # Get all the contrasts the user specified in their StructuredExperiment- if any

  getAllContrasts <- reactive({
    if (has_slot_data(eselist, "contrasts")) {
      contrasts <- eselist@contrasts

      contrasts <- lapply(contrasts, function(cont) {
        if (is.null(names(cont))) {
          names(cont) <- c("Variable", "Group.1", "Group.2")
        }
        cont
      })

      names(contrasts) <- as.character(seq_along(contrasts))
      contrasts
    } else {
      NULL
    }
  })

  # Get a named vector of integers for contrasts, to be used in field etc

  getAllContrastsNumbers <- reactive({
    contrasts <- getAllContrasts()
    contrast_names <- makeContrastNamesFor(contrasts)

    if (!is.null(contrasts)) {
      structure(names(contrasts), names = contrast_names)
    } else {
      NULL
    }
  })

  # Get list describing, for each contrast, the samples on each side

  getContrastSamples <- reactive({
    ese <- selectmatrix_reactives$getExperiment()
    coldata <- selectmatrix_reactives$selectColData()
    contrasts <- getAllContrasts()

    lapply(contrasts, function(c) {
      list(colnames(ese)[coldata[[c[["Variable"]]]] == c[["Group.1"]]], colnames(ese)[coldata[[c[["Variable"]]]] == c[["Group.2"]]])
    })
  })

  # Test for the presence of pre-computed fold changes (e.g. from modelling)

  fcsAvailable <- reactive({
    assay <- selectmatrix_reactives$getAssay()
    ese <- selectmatrix_reactives$getExperiment()

    has_slot_data(ese, "contrast_stats") && assay %in% names(ese@contrast_stats) && "fold_changes" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$fold_changes)
  })

  # Report the scale that pre-computed fold changes were interpreted as when
  # they were read in (see resolve_foldchange_scale()). Fold changes
  # calculated on the fly via foldChange() are always linear.

  getFoldChangeScale <- reactive({
    if (!fcsAvailable()) {
      return("linear")
    }

    assay <- selectmatrix_reactives$getAssay()
    ese <- selectmatrix_reactives$getExperiment()

    scale <- attr(ese@contrast_stats[[assay]]$fold_changes, "fold_change_scale")
    if (is.null(scale)) "unspecified" else scale
  })

  # Test for the presence of p values in the input object

  pvalsAvailable <- reactive({
    assay <- selectmatrix_reactives$getAssay()
    ese <- selectmatrix_reactives$getExperiment()

    has_slot_data(ese, "contrast_stats") && assay %in% names(ese@contrast_stats) && "pvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$pvals)
  })

  # Test for the presence of q values in the input object

  qvalsAvailable <- reactive({
    assay <- selectmatrix_reactives$getAssay()
    ese <- selectmatrix_reactives$getExperiment()

    has_slot_data(ese, "contrast_stats") && assay %in% names(ese@contrast_stats) && "qvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$qvals)
  })

  list(
    getSummaries = getSummaries, getAllContrasts = getAllContrasts, getAllContrastsNumbers = getAllContrastsNumbers,
    getContrastSamples = getContrastSamples, fcsAvailable = fcsAvailable, getFoldChangeScale = getFoldChangeScale,
    pvalsAvailable = pvalsAvailable, qvalsAvailable = qvalsAvailable
  )
}

# Shared by contrastEnumeration()'s getAllContrastsNumbers and contrastNaming()'s
# makeContrastNames - both build the same "Variable: Group.2 vs Group.1 (extras)"
# label from a contrasts list, but the former needs it before contrastNaming()
# exists yet (it only depends on getAllContrasts). Kept as a plain function
# (not a reactive) since it's pure given its input.
makeContrastNamesFor <- function(contrasts) {
  lapply(contrasts, function(x) {
    x <- x[!names(x) %in% "id"]
    contrast_name <- paste(prettifyVariablename(x["Variable"]), paste(x["Group.2"], x["Group.1"], sep = " vs "), sep = ": ")
    extras <- setdiff(names(x), c("Variable", "Group.1", "Group.2"))
    if (length(extras) > 0) {
      suffix <- paste0("(", paste(paste(extras, x[extras], sep = ":"), collapse = ","), ")")
      contrast_name <- paste(contrast_name, suffix)
    }
    contrast_name
  })
}

#' Build display names for contrasts
#'
#' @param getAllContrasts Reactive returning the raw contrasts list, from
#'   \code{contrastEnumeration}
#'
#' @return A list of reactives: \code{makeContrastNames} (human-readable),
#'   \code{makeSafeContrastNames} (safe for use as e.g. plot trace names)
#' @noRd
contrastNaming <- function(getAllContrasts) {
  makeContrastNames <- reactive({
    makeContrastNamesFor(getAllContrasts())
  })

  # Make safe set of names to be used where spaces etc not allowed

  makeSafeContrastNames <- reactive({
    contrasts <- getAllContrasts()
    names <- lapply(contrasts, function(x) paste(ucfirst(prettifyVariablename(x["Variable"])), paste(ucfirst(x["Group.2"]), ucfirst(x["Group.1"]), sep = "_vs_"), sep = "."))
    names <- lapply(names, function(name) {
      name <- sub("\\+", "_POS_", name)
      name <- sub("\\-", "_NEG_", name)
    })
    names
  })

  list(makeContrastNames = makeContrastNames, makeSafeContrastNames = makeSafeContrastNames)
}

#' The dynamic contrast filter-set engine
#'
#' Owns everything about the progressively-addable filter sets: inserting and
#' removing filter set UI, tracking their current values in
#' \code{filterset_values} (a reactive-bound list mutated by per-field
#' observers), rebuilding on assay change, and bookmarking/restoring that
#' state. Also owns the accessors that just read \code{filterset_values} (the
#' "form value" reactives), since they're only meaningful together with this
#' state and folding them in here removes what would otherwise be a forward
#' reference (the insert observer and \code{output$combine_operator_ui} use
#' \code{getFilterRows()}/\code{getSelectedContrastNumbers()} before those
#' would be defined, if they lived in a separate stage called later).
#'
#' @param ns,input,output,session Module namespace function and Shiny objects
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#' @param multiple,show_controls,default_foldchange,default_pval,default_qval,select_all_contrasts
#'   Passed through from \code{contrasts()}'s own arguments
#' @param getAllContrasts,getAllContrastsNumbers Reactives from
#'   \code{contrastEnumeration}
#' @param fcsAvailable,pvalsAvailable,qvalsAvailable Reactives from
#'   \code{contrastEnumeration}, used to decide whether a filter set's
#'   cardinality fields are required
#'
#' @return A list of reactives: \code{getFilterRows},
#'   \code{getSelectedContrastNumbers}, \code{getFoldChange},
#'   \code{getFoldChangeCard}, \code{getQval}, \code{getQvalCard},
#'   \code{getPval}, \code{getPvalCard}, \code{getFilterSetCombinationOperator},
#'   and \code{getFilterSetValues} (a raw accessor to the whole
#'   \code{filterset_values} list, for \code{contrastQuerySummary})
#' @noRd
contrastFilterSetEngine <- function(ns, input, output, session, selectmatrix_reactives,
                                     multiple, show_controls, default_foldchange, default_pval, default_qval, select_all_contrasts,
                                     getAllContrasts, getAllContrastsNumbers, fcsAvailable, pvalsAvailable, qvalsAvailable) {
  ########################################################################### Rendering the contrast control filter sets

  inserted <- c() # Stores the list of inserted filter sets
  filterset_values <- list() # Stores the list of values in the filter set
  makeReactiveBinding("filterset_values") # Make the stored values reactive

  filter_observers <- list()

  # insert_more is a bare counter the insert observer depends on, so restored
  # filter sets beyond the first can be re-inserted one at a time.

  restored_filtersets <- NULL
  insert_more <- reactiveVal(0)

  # Get current value of field which determines if the table should be filtered at all.

  getFilterRows <- reactive({
    req(!is.null(input$filterRows))
    as.logical(input$filterRows)
  })

  # Seed a freshly-inserted filter set's fields from bookmarked values. The
  # per-field observers created alongside the set then propagate these into
  # filterset_values, re-establishing the normal dependency chain.

  applyRestoredFilterSet <- function(index, vals) {
    if (!is.null(vals$contrasts)) {
      updateSelectInput(session, paste0("contrasts", index), selected = vals$contrasts)
    }
    for (field in c("p_value", "q_value", "fold_change")) {
      if (!is.null(vals[[field]])) {
        updateNumericInput(session, paste0(field, index), value = as.numeric(vals[[field]]))
      }
      card <- paste0(field, "_card")
      if (!is.null(vals[[card]])) {
        updateSelectInput(session, paste0(card, index), selected = vals[[card]])
      }
    }
  }

  # This observer adds a set of filters on the first page load and when the assocaited button is clicked.

  observeEvent(
    {
      selectmatrix_reactives$selectMatrix()
      input$insertBtn
      insert_more()
    },
    {
      ese <- selectmatrix_reactives$getExperiment()
      contrasts <- getAllContrasts()
      contrast_numbers <- getAllContrastsNumbers()
      assay <- selectmatrix_reactives$getAssay()
      coldata <- selectmatrix_reactives$selectColData()

      # Restrict contrasts to those valid for the input matrix

      valid_contrasts <- unlist(lapply(contrasts, function(cont) {
        all(c(cont[["Group.1"]], cont[["Group.2"]]) %in% coldata[[cont[["Variable"]]]])
      }))
      contrasts <- contrasts[valid_contrasts]
      contrast_numbers <- contrast_numbers[valid_contrasts]

      # btn keeps track of how many filter sets have been added

      btn <- length(inserted)

      # Call makeContrastFilterSet() to generate a set of filters, and add to the UI with insertUI()

      insertUI(selector = paste0("#", ns("contrasts-placeholder")), where = "beforeEnd", ui = makeContrastFilterSet(ns, ese, assay, contrasts, contrast_numbers,
        multiple = multiple, show_controls = show_controls, default_foldchange = default_foldchange, default_pval = default_pval, default_qval = default_qval,
        filter_rows = getFilterRows(), index = btn, select_all_contrasts = select_all_contrasts
      ))

      # Record the ID of the added filter set

      inserted <<- c(inserted, paste0("contrast", btn))

      # Now add observers for each new element generated by makeControlFilterSet(). When they fire, these observers will add the filter values to the reactive
      # filterset_values. When this is referenced (by e.g. getFoldChange()), the dependency chain is established such that outputs are refreshed when the
      # dynamically added fields are altered.

      filterId <- paste0("filter", btn)

      filter_observers[[filterId]] <<- lapply(c("contrasts", "fold_change", "q_value", "p_value", "fold_change_card", "q_value_card", "p_value_card"), function(field) {
        filter_field_id <- paste0(field, btn)
        observeEvent(input[[filter_field_id]],
          {
            req(input[[filter_field_id]])
            if (is.null(filterset_values[[filterId]])) {
              filterset_values[[filterId]] <<- list()
            }

            if (length(input[[filter_field_id]]) == 1 && input[[filter_field_id]] == "NULL") {
              filterset_values[[filterId]][[field]] <<- NULL
            } else {
              filterset_values[[filterId]][[field]] <<- input[[filter_field_id]]
            }

            # filterset_values[[filterId]][[paste0(field, 'card')]] <<- input[[paste0(filter_field_id, 'card')]]
          },
          ignoreNULL = FALSE
        )
      })

      # Re-seed this set from bookmarked values on every (re)creation while a
      # restore is pending. The assay-change reset (priority 2) rebuilds the
      # sets whenever selectMatrix() changes as inputs restore, so applying
      # once would be wiped; re-applying here survives that churn until
      # onRestored clears the pending state.

      if (!is.null(restored_filtersets)) {
        if (btn < length(restored_filtersets)) {
          applyRestoredFilterSet(btn, restored_filtersets[[btn + 1]])
        }
        if (length(inserted) < length(restored_filtersets)) {
          insert_more(insert_more() + 1)
        }
      }
    },
    ignoreNULL = FALSE,
    priority = 1
  )

  # This observer removes a filter set when the 'remove' button is clicked.  This removes both the UI element and its stored values in filterset_values

  observeEvent(input$removeBtn, {
    if (length(inserted) > 1) {
      removeUI(selector = paste0("#", inserted[length(inserted)]))
      inserted <<- inserted[-length(inserted)]
      filterset_values[[length(filterset_values)]] <<- NULL
    }
  })

  # When a new assay is selected, or when the input matrix is otherwise changed, we need to rebuild the inputs

  observeEvent(selectmatrix_reactives$selectMatrix(),
    {
      if (length(inserted) > 0) {
        lapply(names(filterset_values), function(filterId) {
          lapply(names(filterset_values[[filterId]]), function(field) {
            filterset_values[[filterId]][[field]] <<- NULL
          })
          filterset_values[[filterId]] <<- NULL
        })
        removeUI(selector = ".shinyngs-contrast", multiple = TRUE, immediate = TRUE)
        inserted <<- c()
      }
    },
    priority = 2
  )

  # The combine_operator field is only necessary with more than one filter set

  output$combine_operator_ui <- renderUI({
    scn <- getSelectedContrastNumbers()
    if (length(scn) > 1) {
      inlineField(selectInput(ns("combine_operator"), NULL, c(and = "intersect", or = "union")),
        label = "Combine using", 6,
        tooltip = "'Intersect' requires rows to satisfy every selected contrast filter; 'union' includes rows matching any one of them."
      )
    } else {
      hiddenInput(ns("combine_operator"), "intersect")
    }
  })

  ########################################################################### Accessors for form values

  # Get the indices of the currently selected contrasts for each filter set by querying filterset_values.

  getSelectedContrastNumbers <- reactive({
    req(length(filterset_values) > 0)
    lapply(filterset_values, function(x) x$contrasts)
  })

  # Fetch the values from all the fold change filters

  getFoldChange <- reactive({
    req(length(filterset_values) > 0)
    unlist(lapply(filterset_values, function(x) x$fold_change))
  })

  # Fetch the values from all the fold change cardinality filters

  getFoldChangeCard <- reactive({
    req(length(filterset_values) > 0)
    card <- unlist(lapply(filterset_values, function(x) x$fold_change_card))
    if (fcsAvailable()) {
      req(length(card) > 0)
    }
    card
  })

  # Get current value of the q value filter

  getQval <- reactive({
    req(length(filterset_values) > 0)
    unlist(lapply(filterset_values, function(x) x$q_value))
  })

  # Get current value of the q value cardinality filter

  getQvalCard <- reactive({
    req(length(filterset_values) > 0)
    card <- unlist(lapply(filterset_values, function(x) x$q_value_card))
    if (qvalsAvailable()) {
      req(length(card) > 0)
    }
    card
  })

  # Get current value of the p value filter

  getPval <- reactive({
    req(length(filterset_values) > 0)
    unlist(lapply(filterset_values, function(x) x$p_value))
  })

  # Get current value of the p value cardinality filter

  getPvalCard <- reactive({
    req(length(filterset_values) > 0)
    card <- unlist(lapply(filterset_values, function(x) x$p_value_card))
    if (pvalsAvailable()) {
      req(length(card) > 0)
    }
    card
  })

  # Get method for combining filters

  getFilterSetCombinationOperator <- reactive({
    req(input$combine_operator)
    input$combine_operator
  })

  ########################################################################### Bookmarking of the dynamically-built filter sets

  # The filter-set inputs are inserted at runtime, so a bookmark URL cannot
  # capture or restore them by itself. Snapshot filterset_values on bookmark
  # and stash it on restore for the insert observer to replay. Keys are
  # namespaced so multiple contrasts instances in one app don't collide.

  onBookmark(function(state) {
    state$values[[session$ns("contrast_filtersets")]] <- filterset_values
  })

  onRestore(function(state) {
    saved <- state$values[[session$ns("contrast_filtersets")]]
    if (!is.null(saved) && length(saved) > 0) {
      restored_filtersets <<- unname(saved)
    }
  })

  # Close the restore window once the restore flush has settled, so later
  # user-driven filter sets start from defaults rather than the bookmarked
  # values.

  onRestored(function(state) {
    restored_filtersets <<- NULL
  })

  list(
    getFilterRows = getFilterRows, getSelectedContrastNumbers = getSelectedContrastNumbers,
    getFoldChange = getFoldChange, getFoldChangeCard = getFoldChangeCard,
    getQval = getQval, getQvalCard = getQvalCard, getPval = getPval, getPvalCard = getPvalCard,
    getFilterSetCombinationOperator = getFilterSetCombinationOperator,
    getFilterSetValues = function() filterset_values
  )
}

#' Build per-contrast summary tables (means, fold changes, p/q values)
#'
#' Main table generation: for every contrast, a table of the group summary
#' stats and fold change, with pre-computed or on-the-fly fold changes/p/q
#' values, followed by subsetting those tables down to the rows of the
#' currently selected input matrix.
#'
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#' @param getSummaries,getAllContrasts,getAllContrastsNumbers,fcsAvailable,pvalsAvailable,qvalsAvailable
#'   Reactives from \code{contrastEnumeration}
#'
#' @return A list of reactives: \code{contrastsTables},
#'   \code{contrastsTablesToMatchMatrix}
#' @noRd
contrastTableBuilder <- function(selectmatrix_reactives, getSummaries, getAllContrasts, getAllContrastsNumbers, fcsAvailable, pvalsAvailable, qvalsAvailable) {
  # Main function for returning the table of contrast information. Means, fold changes calculated on the fly, p/q values must be supplied in a 'contrast_stats' slot
  # of the ExploratorySummarizedExperiment. Make a summary table for every contrast. This data can then be re-used when processing filter sets.

  contrastsTables <- reactive({
    matrix <- selectmatrix_reactives$selectMatrix()

    ese <- selectmatrix_reactives$getExperiment()
    summaries <- getSummaries()
    contrasts <- getAllContrasts()
    assay <- selectmatrix_reactives$getAssay()

    # There can be a mismatch between the conrasts and summaries as we adjust the input matrix. Wait for updates to finish before making the table.

    # validate(need(all(unlist(lapply(selected_contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))), 'Matching summaries and contrasts'))

    withProgress(message = "Calculating contrast tables", value = 0, {
      contrast_tables <- lapply(names(contrasts), function(c) {
        cont <- contrasts[[c]]

        smry1 <- summaries[[cont[["Variable"]]]][, cont[["Group.1"]]]
        smry2 <- summaries[[cont[["Variable"]]]][, cont[["Group.2"]]]

        ct <- data.frame(cont[["Variable"]], cont[["Group.1"]], cont[["Group.2"]], round(smry1, 2), round(smry2, 2), row.names = names(smry1))
        names(ct) <- c("Variable", "Condition 1", "Condition 2", "Average 1", "Average 2")

        # Use pre-computed fold changes where provided.

        if (fcsAvailable()) {
          fcs <- ese@contrast_stats[[assay]]$fold_changes
          ct[["Fold change"]] <- round(fcs[match(rownames(ct), rownames(fcs)), as.numeric(c)], 2)
        } else {
          ct[["Fold change"]] <- round(foldChange(smry1, smry2), 2)
        }

        if (pvalsAvailable()) {
          pvals <- ese@contrast_stats[[assay]]$pvals
          ct[["p value"]] <- signif(pvals[match(rownames(ct), rownames(pvals)), as.numeric(c)], 5)
        }

        if (qvalsAvailable()) {
          qvals <- ese@contrast_stats[[assay]]$qvals
          ct[["q value"]] <- signif(qvals[match(rownames(ct), rownames(qvals)), as.numeric(c)], 5)
        }

        ct
      })
    })

    names(contrast_tables) <- getAllContrastsNumbers()
    contrast_tables
  })

  ########################################################################### Subsetting using the rows in the input matrix. This does NOT involve the filters from this module, but simply subsets the base data to the rows pertinent
  ########################################################################### to the input matrix.

  # Get contrasts tables with rows reflecting the input matrix

  contrastsTablesToMatchMatrix <- reactive({
    contrast_tables <- contrastsTables()
    matrix <- selectmatrix_reactives$selectMatrix()

    lapply(contrast_tables, function(ct) {
      ct[rownames(matrix), ]
    })
  })

  list(contrastsTables = contrastsTables, contrastsTablesToMatchMatrix = contrastsTablesToMatchMatrix)
}

#' Resolve the currently-selected contrasts and their names/samples
#'
#' @param getSelectedContrastNumbers Reactive from \code{contrastFilterSetEngine}
#' @param getAllContrasts,getContrastSamples Reactives from
#'   \code{contrastEnumeration}
#' @param makeContrastNames,makeSafeContrastNames Reactives from
#'   \code{contrastNaming}
#'
#' @return A list of reactives: \code{getSelectedContrasts},
#'   \code{getSelectedContrastNames}, \code{getSafeSelectedContrastNames},
#'   \code{getSelectedContrastSamples}, \code{singleContrast}
#' @noRd
contrastSelection <- function(getSelectedContrastNumbers, getAllContrasts, getContrastSamples, makeContrastNames, makeSafeContrastNames) {
  # Get the actual contrasts to which the numbers from the interface pertain

  getSelectedContrasts <- reactive({
    scn <- getSelectedContrastNumbers()
    all_contrasts <- getAllContrasts()

    lapply(scn, function(s) {
      all_contrasts[s]
    })
  })

  # Get the name of the currently selected contrast

  getSelectedContrastNames <- reactive({
    contrast_names <- makeContrastNames()
    lapply(getSelectedContrastNumbers(), function(x) contrast_names[x])
  })

  # The same, but with safe names that won't get mangled by plotting etc

  getSafeSelectedContrastNames <- reactive({
    contrast_names <- makeSafeContrastNames()

    lapply(getSelectedContrastNumbers(), function(scns) {
      contrast_names[scns]
    })
  })

  # Get samples for currently selected contrast(s), one entry per filter set
  # (matching the nesting of getSelectedContrasts() above): each entry is
  # itself a list of list(group_1_samples, group_2_samples), one per
  # contrast selected within that filter set

  getSelectedContrastSamples <- reactive({
    contrast_samples <- getContrastSamples()
    scn <- getSelectedContrastNumbers()

    lapply(scn, function(s) {
      contrast_samples[s]
    })
  })

  # If we're only looking at a single contrast filter with a single contrast (e.g. for a fold change plot etc), then we can simplify.

  singleContrast <- reactive({
    selected_contrasts <- getSelectedContrastNumbers()
    length(selected_contrasts) == 1 && length(selected_contrasts[[1]]) == 1
  })

  list(
    getSelectedContrasts = getSelectedContrasts, getSelectedContrastNames = getSelectedContrastNames,
    getSafeSelectedContrastNames = getSafeSelectedContrastNames, getSelectedContrastSamples = getSelectedContrastSamples,
    singleContrast = singleContrast
  )
}

#' Apply the user's filter-set selections and cardinality filters
#'
#' Filters the per-contrast tables down to the selected contrasts, then (if
#' row filtering is enabled) to the rows meeting the fold change/p/q value
#' cardinality filters, and finally intersects/unions the resulting feature
#' sets across filter sets.
#'
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#' @param getSelectedContrastNumbers,getFilterRows,getFilterSetCombinationOperator
#'   Reactives from \code{contrastFilterSetEngine}
#' @param contrastsTablesToMatchMatrix Reactive from \code{contrastTableBuilder}
#' @param singleContrast Reactive from \code{contrastSelection}
#' @param getFoldChange,getFoldChangeCard,getPval,getPvalCard,getQval,getQvalCard
#'   Reactives from \code{contrastFilterSetEngine}
#' @param pvalsAvailable,qvalsAvailable Reactives from \code{contrastEnumeration}
#'
#' @return A list of reactives: \code{selectedContrastsTables},
#'   \code{filteredContrastsTables}, \code{selectFilterFinalFeatures},
#'   \code{selectFinalFeatures}
#' @noRd
contrastFiltering <- function(selectmatrix_reactives, getSelectedContrastNumbers, contrastsTablesToMatchMatrix, singleContrast,
                               getFoldChange, getFoldChangeCard, getPval, getPvalCard, getQval, getQvalCard,
                               pvalsAvailable, qvalsAvailable, getFilterRows, getFilterSetCombinationOperator) {
  # Filter contrasts tables down to the contrasts of interest

  selectedContrastsTables <- reactive({
    selected_contrasts <- getSelectedContrastNumbers()
    contrast_tables <- contrastsTablesToMatchMatrix()

    req(selected_contrasts, contrast_tables)

    # Selected contrasts is a list, one for each filter set. Each one can have multiple contrasts

    withProgress(message = "Filtering to specified features", value = 0, {
      lapply(selected_contrasts, function(scs_set) {
        lapply(scs_set, function(s) {
          ct <- contrast_tables[[s]]
          if (singleContrast()) {
            simplifyContrastTable(ct)
          } else {
            ct
          }
        })
      })
    })
  })

  # Apply user filters to results of contrastsTables(). Called on first page load and on subsequent clicks of 'Apply'.

  filteredContrastsTables <- reactive({
    selected_contrasts_tables <- selectedContrastsTables()
    req(length(selected_contrasts_tables) > 0)

    if (getFilterRows()) {
      ese <- selectmatrix_reactives$getExperiment()
      assay <- selectmatrix_reactives$getAssay()

      fold_change <- getFoldChange()
      fold_change_card <- getFoldChangeCard()
      p_value <- getPval()
      p_value_card <- getPvalCard()
      q_value <- getQval()
      q_value_card <- getQvalCard()

      withProgress(message = "Applying filters", value = 0, {
        fcts <- lapply(seq_along(selected_contrasts_tables), function(i) {
          sct <- selected_contrasts_tables[[i]]

          lapply(sct, function(s) {
            filter <- evaluateCardinalFilter(s[["Fold change"]], fold_change_card[i], fold_change[i])

            if (pvalsAvailable()) {
              filter <- filter & evaluateCardinalFilter(s[["p value"]], p_value_card[i], p_value[i])
            }

            if (qvalsAvailable()) {
              filter <- filter & evaluateCardinalFilter(s[["q value"]], q_value_card[i], q_value[i])
            }

            s[filter, , drop = FALSE]
          })
        })
      })
    } else {
      fcts <- selected_contrasts_tables
    }
    fcts
  })

  # Find the list of features that result from combining all the filters

  selectFilterFinalFeatures <- reactive({
    filtered_contrasts_tables <- filteredContrastsTables()
    validate(need(length(filtered_contrasts_tables) > 0, "Waiting for filtered contrasts tables"))

    lapply(filtered_contrasts_tables, function(fcts) {
      Reduce(intersect, lapply(fcts, function(fct) {
        rownames(fct)
      }))
    })
  })

  selectFinalFeatures <- reactive({
    filter_final_features <- selectFilterFinalFeatures()

    withProgress(message = "Selecting final feature set", value = 0, {
      comb_op <- getFilterSetCombinationOperator()
      Reduce(get(comb_op), filter_final_features)
    })
  })

  list(
    selectedContrastsTables = selectedContrastsTables, filteredContrastsTables = filteredContrastsTables,
    selectFilterFinalFeatures = selectFilterFinalFeatures, selectFinalFeatures = selectFinalFeatures
  )
}

#' Label and link the final filtered contrasts table
#'
#' @param eselist ExploratorySummarizedExperimentList object
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#' @param selectFinalFeatures,filteredContrastsTables Reactives from
#'   \code{contrastFiltering}
#'
#' @return A list of reactives: \code{labelledContrastsTable},
#'   \code{linkedLabelledContrastsTable}
#' @noRd
contrastLabelling <- function(eselist, selectmatrix_reactives, selectFinalFeatures, filteredContrastsTables) {
  # The output of filteredContrastsTables() are significant results for each filter set, and each contrast within those.

  labelledContrastsTable <- reactive({
    ese <- selectmatrix_reactives$getExperiment()
    sff <- selectFinalFeatures()

    filtered_contrast_tables <- filteredContrastsTables()

    metafields <- c()
    if (!is.null(selectmatrix_reactives$getMetafields)) {
      metafields <- selectmatrix_reactives$getMetafields()
    }

    withProgress(message = "Making labelled table", value = 0, {
      final_contrasts_table <- unique(do.call(rbind, lapply(filtered_contrast_tables, function(fcts) {
        do.call(rbind, lapply(fcts, function(fct) {
          labelMatrix(fct[sff, , drop = FALSE], ese = ese, metafields = metafields)
        }))
      })))
    })
  })

  # Use labelledContrastsTable to get the labelled matrix and add some links.

  linkedLabelledContrastsTable <- reactive({
    lct <- labelledContrastsTable()
    if (has_slot_data(eselist, "url_roots")) {
      lct <- linkMatrix(lct, eselist@url_roots)
    }
    lct
  })

  list(labelledContrastsTable = labelledContrastsTable, linkedLabelledContrastsTable = linkedLabelledContrastsTable)
}

#' Summarise the query and its results for the user
#'
#' Builds the differential-count summary table, the human-readable query
#' strings describing each filter set, the query summary table, and renders
#' \code{output$summary}.
#'
#' @param output The module's \code{output} object
#' @param selectmatrix_reactives Reactives from the \code{selectmatrix} module
#' @param filteredContrastsTables,selectFilterFinalFeatures,selectFinalFeatures
#'   Reactives from \code{contrastFiltering}
#' @param getSelectedContrasts Reactive from \code{contrastSelection}
#' @param makeContrastNames Reactive from \code{contrastNaming}
#' @param getFilterSetValues Raw \code{filterset_values} accessor from
#'   \code{contrastFilterSetEngine}
#' @param getFilterSetCombinationOperator Reactive from
#'   \code{contrastFilterSetEngine}
#'
#' @return A list of reactives: \code{makeDifferentialSetSummary},
#'   \code{getQueryStrings}
#' @noRd
contrastQuerySummary <- function(output, selectmatrix_reactives, filteredContrastsTables, getSelectedContrasts, makeContrastNames,
                                  getFilterSetValues, selectFilterFinalFeatures, selectFinalFeatures, getFilterSetCombinationOperator) {
  # A summary table of differential expression

  makeDifferentialSetSummary <- reactive({
    fcts <- filteredContrastsTables()
    selected_contrasts <- getSelectedContrasts()
    queries <- getQueryStrings()
    eid <- selectmatrix_reactives$getExperimentId()

    summaries <- lapply(seq_along(fcts), function(i) {
      summary <- data.frame(cbind(query = queries[i], do.call(rbind, selected_contrasts[[i]])))
      colnames(summary) <- c("Query", "Variable", "group 1", "group 2")
      summary[[paste0("Differential ", eid, "s (up)")]] <- unlist(lapply(fcts[[i]], function(x) sum(x[, "Fold change"] > 0)))
      summary[[paste0("Differential ", eid, "s (down)")]] <- unlist(lapply(fcts[[i]], function(x) sum(x[, "Fold change"] < 0)))
      summary[[paste0("Differential ", eid, "s (total)")]] <- unlist(lapply(fcts[[i]], nrow))

      summary
    })

    if (length(summaries) == 1) {
      summaries[[1]][, -1]
    } else {
      do.call(rbind, summaries)
    }
  })

  getQueryStrings <- reactive({
    contrast_names <- makeContrastNames()

    # Get the current filters, getting the name for the contrast(s)

    fvs <- lapply(getFilterSetValues(), function(fv) {
      fv$contrasts <- paste(contrast_names[fv$contrasts], collapse = ", ")
      fv
    })

    unlist(lapply(fvs, function(x) {
      paste("<p>", paste(unlist(lapply(grep("card", names(x[-1]), invert = TRUE, value = TRUE), function(y) {
        paste(y, x[[paste0(y, "_card")]], x[[y]])
      })), collapse = " AND "), "in <i>", x[[1]], "</i></p>")
    }))
  })

  makeQuerySummary <- reactive({
    filters <- getQueryStrings()

    # Get the list of features resulting from each filter

    filter_final_features <- selectFilterFinalFeatures()

    # Make a table of the number of features resulting from each filter

    query_summary <- data.frame(filter = filters, features = unlist(lapply(filter_final_features, length)))

    exp_id <- selectmatrix_reactives$getExperimentId()
    colnames(query_summary)[2] <- paste0(exp_id, "s")

    # Convert to labels

    labelfield <- selectmatrix_reactives$getLabelField()
    if (!is.null(labelfield)) {
      ese <- selectmatrix_reactives$getExperiment()
      filter_final_labels <- lapply(filter_final_features, function(x) convertIds(x, ese, labelfield))

      query_summary[[paste0(labelfield, "s")]] <- unlist(lapply(filter_final_labels, function(x) length(unique(x))))
    }

    query_summary
  })

  ########################################################################### Tell the user something about the query and its results

  output$summary <- renderUI({
    query_summary <- makeQuerySummary()
    comb_op <- getFilterSetCombinationOperator()
    operator <- ifelse(comb_op == "intersect", "AND", "OR")
    query_summary[-1, 1] <- paste0("<p><b>", operator, "</b></p>", query_summary[-1, 1])

    if (ncol(query_summary) == 2) {
      column_widths <- c(8, 4)
    } else {
      column_widths <- c(6, 3, 3)
    }

    makeFluidRow <- function(row) {
      do.call(fluidRow, lapply(seq_along(row), function(r) {
        column(column_widths[r], HTML(row[r]))
      }))
    }

    summary_bits <- list(h4("Query Summary"), makeFluidRow(prettifyVariablename(colnames(query_summary))), br(), apply(query_summary, 1, makeFluidRow))

    if (nrow(query_summary) > 1) {
      sff_in <- selectFinalFeatures()
      sff <- unique(sff_in)
      ese <- selectmatrix_reactives$getExperiment()

      summary_row <- c(comb_op, length(sff))

      labelfield <- selectmatrix_reactives$getLabelField()
      if (!is.null(labelfield)) {
        labels <- convertIds(ids = sff, ese, labelfield)
        summary_row <- c(summary_row, length(unique(labels[!is.na(labels)])))
      }

      summary_bits <- c(summary_bits, list(hr(), makeFluidRow(summary_row)))
    }

    list(tags$br(), do.call(wellPanel, summary_bits))
  })

  list(makeDifferentialSetSummary = makeDifferentialSetSummary, getQueryStrings = getQueryStrings)
}

#' Calculate fold change between two vectors
#'
#' @param vec1 First vector
#' @param vec2 Second vector
#'
#' @return Vector of fold changes
#'
#' @export
#' @examples
#' foldChange(c(2, 2, 2), c(2, 4, 8))
#'
foldChange <- function(vec1, vec2) {
  fc <- vec2 / vec1
  fc[vec1 == vec2] <- 1
  fc[which(fc < 1)] <- -1 / fc[which(fc < 1)]
  fc
}

#' Make a complete set of filters for a contrast: the contrast itself, fold
#' change, and where applicable p- and q- values.
#'
#' @param ns A namespace function (created with \code{\link[shiny]{NS}} to be
#' used in creating field IDs.
#' @param ese ExploratorySummarizedExperiment object
#' @param assay Assay in \code{ese}
#' @param contrasts A list of lists specifying contrasts.
#' @param contrast_numbers A named vector of indices corresponding to
#' \code{contrasts}.
#' @param multiple Allow multiple contrasts to be selected? Passed to
#' \code{\link{makeContrastControl}}.
#' @param show_controls Show controls? Setting to false will cause them to be
#' hidden.
#' @param default_foldchange Default value for the fold change field
#' @param default_pval Default value for the p value field
#' @param default_qval Default value for the q value field
#' @param index Index. Will be used to differentiate mutiple copies of the
#' field set.
#' @param filter_rows Use fold change and p value etc to filter values?
#' @param select_all_contrasts Select all contrasts by default?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()

makeContrastFilterSet <- function(ns, ese, assay, contrasts, contrast_numbers, multiple, show_controls, default_foldchange = 2, default_pval = 0.05, default_qval = 0.1,
                                  index = "", filter_rows = TRUE, select_all_contrasts = FALSE) {
  contrast_field_set <- list(makeContrastControl(ns(paste0("contrasts", index)), contrasts, contrast_numbers,
    multiple = multiple, show_controls = show_controls,
    select_all = select_all_contrasts
  ))

  if (filter_rows) {
    # p value field

    if ("pvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$pvals)) {
      pval_field <- cardinalNumericField(ns(paste0("p_value", index)), ns(paste0("p_value_card", index)),
        value = default_pval, label = "p value", min = 0,
        max = 1, step = 0.01,
        tooltip = "Only include results with an unadjusted p value meeting this threshold and cardinality."
      )
    } else {
      pval_field <- hiddenInput(ns(paste0("p_value", index)), values = "NULL")
      pval_field <- hiddenInput(ns(paste0("p_value_card", index)), values = "NULL")
    }
    contrast_field_set <- c(contrast_field_set, list(pval_field))

    # q value field

    if ("qvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$qvals)) {
      qval_field <- cardinalNumericField(ns(paste0("q_value", index)), ns(paste0("q_value_card", index)),
        value = default_qval, label = "q value", min = 0,
        max = 1, step = 0.01,
        tooltip = "Only include results with a multiple-testing-adjusted q value (FDR) meeting this threshold and cardinality."
      )
    } else {
      qval_field <- hiddenInput(ns(paste0("q_value", index)), values = "NULL")
      qval_field <- hiddenInput(ns(paste0("q_value_card", index)), values = "NULL")
    }
    contrast_field_set <- c(contrast_field_set, list(qval_field))

    fold_change_field <- cardinalNumericField(ns(paste0("fold_change", index)), ns(paste0("fold_change_card", index)),
      value = default_foldchange, label = "fold change",
      cardinality = ">= or <= -", step = 0.5,
      tooltip = "Only include results with a fold change meeting this threshold; '>= or <= -' matches changes of this magnitude in either direction."
    )
    contrast_field_set <- c(contrast_field_set, list(fold_change_field))
  }

  tags$fieldset(id = paste0("contrast", index), contrast_field_set, class = "shinyngs-contrast")
}

#' Make a select field for picking one or more contrasts
#'
#' @param id An id to apply to form elements
#' @param contrasts A list of lists specifying contrasts.
#' @param contrast_numbers A named vector of indices corresponding to
#' \code{contrasts}.
#' @param multiple Allow multiple contrasts to be selected?
#' @param show_controls Show controls? Setting to false will cause them to be
#' hidden.
#' @param select_all Select all contrasts by default?
#'
#' @return output An HTML tag object that can be rendered as HTML using
#' as.character()

makeContrastControl <- function(id, contrasts, contrast_numbers, multiple = FALSE, show_controls = TRUE, select_all = FALSE) {
  if (!is.null(contrast_numbers)) {
    if (multiple) {
      selected <- contrast_numbers
      if (!select_all) {
        selected <- contrast_numbers[1]
      }
      cont_control <- selectInput(id, "Contrast(s):", choices = contrast_numbers, selected = selected, selectize = TRUE, multiple = TRUE)
    } else {
      cont_control <- selectInput(id, "Contrast(s):", contrast_numbers)
    }

    if (!show_controls) {
      cont_control <- shinyjs::hidden(cont_control)
    }
    cont_control
  }
}

#' Simplify a contrast table
#'
#' By default the contrast tables are created with three initial columns to
#' indicate the contrast: the metadata variable and the two values of that
#' variable that define the contrast. But if there is only one contrast then
#' this make the table overly cumbersome, an we can simplify it by simply
#' naming the average column to the values of the contrast variable.
#'
#' @param table Three-column contrast table
#'
#' @return output Simplified table

simplifyContrastTable <- function(table) {
  if (length(unique(table$Variable)) > 1 || length(unique(table[["Condition 1"]])) > 1 || length(unique(table[["Condition 2"]])) > 1) {
    stop("Table represents multiple contrasts, it cannot be simplified.")
  }

  colnames(table)[4:5] <- c(as.character(table[1, "Condition 1"]), as.character(table[1, "Condition 2"]))
  table[, 4:ncol(table)]
}
