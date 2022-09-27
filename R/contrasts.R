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
#' contrastsInput('test')

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
        contrast_filters <- c(list(hiddenInput(ns("dynamic"), 1)), list(helpText("Build up a complex query by adding filters below"), hr()), contrast_filters, 
            list(hr(), uiOutput(ns("combine_operator")), actionButton(ns("insertBtn"), "Add"), HTML("&nbsp;"), actionButton(ns("removeBtn"), "Remove")))
    } else {
        contrast_filters <- pushToList(contrast_filters, uiOutput(ns("combine_operator")))
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
#' contrastsOutput('myid')

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
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
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
#' @import data.table
#' 
#' @examples
#' callModule(contrasts, 'differential', getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)

contrasts <- function(input, output, session, eselist, selectmatrix_reactives = list(), multiple = FALSE, select_all_contrasts = FALSE, show_controls = TRUE, 
    default_foldchange = 2, default_pval = 0.05, default_qval = 0.1) {
    
    ns <- session$ns
    
    unpack.list(selectmatrix_reactives)
    
    getSummaryType <- callModule(summarisematrix, "contrasts")
    
    ########################################################################### Rendering the contrast control filter sets
    
    inserted <- c()  # Stores the list of inserted filter sets
    filterset_values <- list()  # Stores the list of values in the filter set
    makeReactiveBinding("filterset_values")  # Make the stored values reactive
    
    filter_observers <- list()
    
    # This observer adds a set of filters on the first page load and when the assocaited button is clicked.
    
    observeEvent({
        selectMatrix()
        input$insertBtn
    }, {
        
        ese <- getExperiment()
        contrasts <- getAllContrasts()
        contrast_numbers <- getAllContrastsNumbers()
        assay <- getAssay()
        coldata <- selectColData()
        
        # Restrict contrasts to those valid for the input matrix
        
        valid_contrasts <- unlist(lapply(contrasts, function(cont) {
            all(cont[-1] %in% coldata[[cont[1]]])
        }))
        contrasts <- contrasts[valid_contrasts]
        contrast_numbers <- contrast_numbers[valid_contrasts]
        
        # btn keeps track of how many filter sets have been added
        
        btn <- length(inserted)
        
        # Call makeContrastFilterSet() to generate a set of filters, and add to the UI with insertUI()
        
        insertUI(selector = paste0("#", ns("contrasts-placeholder")), where = "beforeEnd", ui = makeContrastFilterSet(ns, ese, assay, contrasts, contrast_numbers, 
            multiple = multiple, show_controls = show_controls, default_foldchange = default_foldchange, default_pval = default_pval, default_qval = default_qval, 
            filter_rows = getFilterRows(), index = btn, select_all_contrasts = select_all_contrasts))
        
        # Record the ID of the added filter set
        
        inserted <<- c(inserted, paste0("contrast", btn))
        
        # Now add observers for each new element generated by makeControlFilterSet(). When they fire, these observers will add the filter values to the reactive
        # filterset_values. When this is referenced (by e.g. getFoldChange()), the dependency chain is established such that outputs are refreshed when the
        # dynamically added fields are altered.
        
        filterId <- paste0("filter", btn)
        
        filter_observers[[filterId]] <<- lapply(c("contrasts", "fold_change", "q_value", "p_value", "fold_change_card", "q_value_card", "p_value_card"), function(field) {
            filter_field_id <- paste0(field, btn)
            observeEvent(input[[filter_field_id]], {
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
            }, ignoreNULL = FALSE)
        })
    }, ignoreNULL = FALSE, priority = 1)
    
    # This observer removes a filter set when the 'remove' button is clicked.  This removes both the UI element and its stored values in filterset_values
    
    observeEvent(input$removeBtn, {
        if (length(inserted) > 1) {
            removeUI(selector = paste0("#", inserted[length(inserted)]))
            inserted <<- inserted[-length(inserted)]
            filterset_values[[length(filterset_values)]] <<- NULL
        }
    })
    
    # When a new assay is selected, or when the input matrix is otherwise changed, we need to rebuild the inputs
    
    observeEvent(selectMatrix(), {
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
    }, priority = 2)
    
    # The combine_operator field is only necessary with more than one filter set
    
    output$combine_operator <- renderUI({
        scn <- getSelectedContrastNumbers()
        if (length(scn) > 1) {
            inlineField(selectInput(ns("combine_operator"), NULL, c(and = "intersect", or = "union")), label = "Combine using", 6)
        } else {
            hiddenInput(ns("combine_operator"), "intersect")
        }
    })
    
    ########################################################################### Accessors for form values
    
    # Get current value of field which determines if the table should be filtered at all.
    
    getFilterRows <- reactive({
        as.logical(input$filterRows)
    })
    
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
        unlist(lapply(filterset_values, function(x) x$fold_change_card))
    })
    
    # Get current value of the q value filter
    
    getQval <- reactive({
        req(length(filterset_values) > 0)
        unlist(lapply(filterset_values, function(x) x$q_value))
    })
    
    # Get current value of the q value cardinality filter
    
    getQvalCard <- reactive({
        req(length(filterset_values) > 0)
        unlist(lapply(filterset_values, function(x) x$q_value_card))
    })
    
    # Get current value of the p value filter
    
    getPval <- reactive({
        req(length(filterset_values) > 0)
        unlist(lapply(filterset_values, function(x) x$p_value))
    })
    
    # Get current value of the p value cardinality filter
    
    getPvalCard <- reactive({
        req(length(filterset_values) > 0)
        unlist(lapply(filterset_values, function(x) x$p_value_card))
    })
    
    # Get method for combining filters
    
    getFilterSetCombinationOperator <- reactive({
        req(input$combine_operator)
        input$combine_operator
    })
    
    # Use presence of combine operator as a flag for use of dynamic contrasts, i.e. the possibility of multiple filter sets
    
    isDynamic <- reactive({
        !is.null(input$dynamic)
    })
    
    ########################################################################### Generate summaries and contrast stats for all contrasts and all rows.  Then the data is handy for subsetting by other functions.  NOTE: this uses ALL
    ########################################################################### rows in the input ExploratorySummarizedExperiment.  The rows in the matrix returned by selectMatrix() will but a subset, but any modifications to the
    ########################################################################### rows used will not necessitate a re-calculation of these basic stats.
    
    # Generate the summary statistic (probably mean) for column groups as defined by the possible contrasts. Other functions can then pick from this output and
    # calculate fold changes etc.
    
    getSummaries <- reactive({
        if (!is.null(getSummaryType())) {
            ese <- getExperiment()
            contrasts <- getAllContrasts()
            matrix <- getAssayMatrix()
            coldata <- data.frame(colData(ese))
            
            validate(need(nrow(matrix) > 0, "Waiting for input matrix"))
            
            contrast_variables <- unique(unlist(lapply(contrasts, function(x) x[1])))
            names(contrast_variables) <- contrast_variables
            
            withProgress(message = paste("Calculating summaries by", getSummaryType()), value = 0, {
                summaries <- lapply(contrast_variables, function(cv) summarizeMatrix(matrix, coldata[[cv]], getSummaryType()))
            })
            
            summaries
        }
    })
    
    # Get all the contrasts the user specified in their StructuredExperiment- if any
    
    getAllContrasts <- reactive({
        if (length(eselist@contrasts) > 0) {
            contrasts <- eselist@contrasts
            names(contrasts) <- as.character(1:length(contrasts))
            contrasts
        } else {
            NULL
        }
    })
    
    # Get a named vector of integers for contrasts, to be used in field etc
    
    getAllContrastsNumbers <- reactive({
        contrasts <- getAllContrasts()
        contrast_names <- makeContrastNames()
        
        if (!is.null(contrasts)) {
            structure(names(contrasts), names = contrast_names)
        } else {
            NULL
        }
    })
    
    # Get list describing, for each contrast, the samples on each side
    
    getContrastSamples <- reactive({
        ese <- getExperiment()
        coldata <- selectColData()
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(c) {
            list(colnames(ese)[coldata[c[1]] == c[2]], colnames(ese)[coldata[c[1]] == c[3]])
        })
    })
    
    # Main function for returning the table of contrast information. Means, fold changes calculated on the fly, p/q values must be supplied in a 'contrast_stats' slot
    # of the ExploratorySummarizedExperiment. Make a summary table for every contrast. This data can then be re-used when processing filter sets.
    
    contrastsTables <- reactive({
        matrix <- selectMatrix()
        
        ese <- getExperiment()
        summaries <- getSummaries()
        contrasts <- getAllContrasts()
        assay <- getAssay()
        
        # There can be a mismatch between the conrasts and summaries as we adjust the input matrix. Wait for updates to finish before making the table.
        
        # validate(need(all(unlist(lapply(selected_contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))), 'Matching summaries and contrasts'))
        
        withProgress(message = "Calculating contrast tables", value = 0, {
            
            contrast_tables <- lapply(names(contrasts), function(c) {
                
                cont <- contrasts[[c]]
                
                smry1 <- summaries[[cont[1]]][, cont[2]]
                smry2 <- summaries[[cont[1]]][, cont[3]]
                
                ct <- data.frame(cont[1], cont[2], cont[3], round(smry1, 2), round(smry2, 2))
                names(ct) <- c("Variable", "Condition 1", "Condition 2", "Average 1", "Average 2")
                
                # Use pre-computed fold changes where provided. 
                
                if (fcsAvailable()) {
                  fcs <- ese@contrast_stats[[assay]]$fold_changes
                  ct[["Fold change"]] <- round(fcs[match(rownames(ct), rownames(fcs)), as.numeric(c)], 2)
                }else{
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
    
    # Test for the presence of pre-computed fold changes (e.g. from modelling)
    
    fcsAvailable <- reactive({
      assay <- getAssay()
      ese <- getExperiment()
      
      length(ese@contrast_stats) > 0 && assay %in% names(ese@contrast_stats) && "fold_changes" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$fold_changes)
    })
    
    # Test for the presence of p values in the input object
    
    pvalsAvailable <- reactive({
        assay <- getAssay()
        ese <- getExperiment()
        
        length(ese@contrast_stats) > 0 && assay %in% names(ese@contrast_stats) && "pvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$pvals)
    })
    
    # Test for the presence of q values in the input object
    
    qvalsAvailable <- reactive({
        assay <- getAssay()
        ese <- getExperiment()
        
        length(ese@contrast_stats) > 0 && assay %in% names(ese@contrast_stats) && "qvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$qvals)
    })
    
    ########################################################################### Subsetting using the rows in the input matrix. This does NOT involve the filters from this module, but simply subsets the base data to the rows pertinent
    ########################################################################### to the input matrix.
    
    # Get contrasts tables with rows reflecting the input matrix
    
    contrastsTablesToMatchMatrix <- reactive({
        contrast_tables <- contrastsTables()
        matrix <- selectMatrix()
        
        lapply(contrast_tables, function(ct) {
            ct[rownames(matrix), ]
        })
    })
    
    ########################################################################### Contrast naming
    
    # Make names for the contrasts
    
    makeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(x) paste(prettifyVariablename(x[1]), paste(x[3], x[2], sep = " vs "), sep = ": "))
    })
    
    # Make safe set of names to be used where spaces etc not allowed
    
    makeSafeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        names <- lapply(contrasts, function(x) paste(ucfirst(prettifyVariablename(x[1])), paste(ucfirst(x[3]), ucfirst(x[2]), sep = "_vs_"), sep = "."))
        names <- lapply(names, function(name) {
            name <- sub("\\+", "_POS_", name)
            name <- sub("\\-", "_NEG_", name)
        })
        names
    })
    
    ########################################################################### Subset contrast-related variables according to filters
    
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
    
    # Get samples for currently selected contrast
    
    getSelectedContrastSamples <- reactive({
        contrast_samples <- getContrastSamples()
        selected_contrasts <- getSelectedContrastNumbers()
        contrast_samples[[selected_contrasts]]
    })
    
    ########################################################################### Process data in response to contrasts filters
    
    # If we're only looking at a single contrast filter with a single contrast (e.g. for a fold change plot etc), then we can simplify.
    
    singleContrast <- reactive({
        selected_contrasts <- getSelectedContrastNumbers()
        length(selected_contrasts) == 1 && length(selected_contrasts[[1]]) == 1
    })
    
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
            ese <- getExperiment()
            assay <- getAssay()
            
            fold_change <- getFoldChange()
            fold_change_card <- getFoldChangeCard()
            p_value <- getPval()
            p_value_card <- getPvalCard()
            q_value <- getQval()
            q_value_card <- getQvalCard()
            
            withProgress(message = "Applying filters", value = 0, {
                
                fcts <- lapply(1:length(selected_contrasts_tables), function(i) {
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
        
        lapply(filtered_contrasts_tables, function(fcts) {
            Reduce(intersect, lapply(fcts, function(fct) {
                rownames(fct)
            }))
        })
    })
    
    selectFinalFeatures <- reactive({
        filter_final_features <- selectFilterFinalFeatures()
        
        withProgress(message = "Selecting final feature set", value = 0, {
            Reduce(get(getFilterSetCombinationOperator()), filter_final_features)
        })
    })
    
    # The output of filteredContrastsTables() are significant results for each filter set, and each contrast within those.
    
    labelledContrastsTable <- reactive({
        
        ese <- getExperiment()
        sff <- selectFinalFeatures()
        
        filtered_contrast_tables <- filteredContrastsTables()
        
        metafields <- c()
        if (!is.null(getMetafields)) {
            metafields <- getMetafields()
        }
        
        withProgress(message = "Making labelled table", value = 0, {
            final_contrasts_table <- unique(rbindlist(lapply(filtered_contrast_tables, function(fcts) {
                rbindlist(lapply(fcts, function(fct) {
                  labelMatrix(fct[sff, , drop = FALSE], ese = ese, metafields = metafields)
                }))
            })))
        })
    })
    
    # Use labelledContrastsTable to get the labelled matrix and add some links.
    
    linkedLabelledContrastsTable <- reactive({
        if (length(eselist@url_roots) > 0) {
            lct <- linkMatrix(labelledContrastsTable(), eselist@url_roots)
        } else {
            lct <- labelledContrastsTable()
        }
        lct
    })
    
    # A summary table of differential expression
    
    makeDifferentialSetSummary <- reactive({
        fcts <- filteredContrastsTables()
        selected_contrasts <- getSelectedContrasts()
        queries <- getQueryStrings()
        eid <- getExperimentId()
        
        summaries <- lapply(1:length(fcts), function(i) {
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
            rbindlist(summaries)
        }
    })
    
    getQueryStrings <- reactive({
        
        contrast_names <- makeContrastNames()
        
        # Get the current filters, getting the name for the contrast(s)
        
        fvs <- lapply(filterset_values, function(fv) {
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
        
        query_summary <- data.frame(filter = filters, features = unlist(lapply(filter_final_features, length)), stringsAsFactors = FALSE)
        
        colnames(query_summary)[2] <- paste0(getExperimentId(), "s")
        
        # Convert to labels
        
        labelfield <- getLabelField()
        if (!is.null(labelfield)) {
            ese <- getExperiment()
            filter_final_labels <- lapply(filter_final_features, function(x) convertIds(x, ese, labelfield))
            
            query_summary[[paste0(labelfield, "s")]] <- unlist(lapply(filter_final_labels, function(x) length(unique(x))))
        }
        
        query_summary
        
    })
    
    ########################################################################### Tell the user something about the query and its results
    
    output$summary <- renderUI({
        
        query_summary <- makeQuerySummary()
        operator <- ifelse(getFilterSetCombinationOperator() == "intersect", "AND", "OR")
        query_summary[-1, 1] <- paste0("<p><b>", operator, "</b></p>", query_summary[-1, 1])
        
        if (ncol(query_summary) == 2) {
            column_widths <- c(8, 4)
        } else {
            column_widths <- c(6, 3, 3)
        }
        
        makeFluidRow <- function(row) {
            do.call(fluidRow, lapply(1:length(row), function(r) {
                column(column_widths[r], HTML(row[r]))
            }))
        }
        
        summary_bits <- list(h4("Query Summary"), makeFluidRow(prettifyVariablename(colnames(query_summary))), br(), apply(query_summary, 1, makeFluidRow))
        
        if (nrow(query_summary) > 1) {
            
            sff <- unique(selectFinalFeatures())
            ese <- getExperiment()
            
            summary_row <- c(getFilterSetCombinationOperator(), length(sff))
            
            labelfield <- getLabelField()
            if (!is.null(labelfield)) {
                labels <- convertIds(ids = sff, ese, labelfield)
                summary_row <- c(summary_row, length(unique(labels[!is.na(labels)])))
            }
            
            summary_bits <- c(summary_bits, list(hr(), makeFluidRow(summary_row)))
        }
        
        list(tags$br(), do.call(wellPanel, summary_bits))
        
    })
    
    ########################################################################### Return the reactive that allow other modules to interact with this one
    
    list(getFoldChange = getFoldChange, getFoldChangeCard = getFoldChangeCard, getQval = getQval, getQvalCard = getQvalCard, getPval = getPval, getPvalCard = getPvalCard, 
        getAllContrasts = getAllContrasts, getSelectedContrasts = getSelectedContrasts, getSelectedContrastNumbers = getSelectedContrastNumbers, getSelectedContrastNames = getSelectedContrastNames, 
        getSafeSelectedContrastNames = getSafeSelectedContrastNames, getContrastSamples = getContrastSamples, getSelectedContrastSamples = getSelectedContrastSamples, 
        contrastsTables = contrastsTables, filteredContrastsTables = filteredContrastsTables, labelledContrastsTable = labelledContrastsTable, linkedLabelledContrastsTable = linkedLabelledContrastsTable, 
        makeDifferentialSetSummary = makeDifferentialSetSummary, getQueryStrings = getQueryStrings, selectedContrastsTables = selectedContrastsTables)
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
#' foldChange(c(2,2,2), c(2,4,8))

foldChange <- function(vec1, vec2) {
    fc <- vec2/vec1
    fc[vec1 == vec2] <- 1
    fc[which(fc < 1)] <- -1/fc[which(fc < 1)]
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
    
    contrast_field_set <- list(makeContrastControl(ns(paste0("contrasts", index)), contrasts, contrast_numbers, multiple = multiple, show_controls = show_controls, 
        select_all = select_all_contrasts))
    
    if (filter_rows) {
        
        # p value field
        
        if ("pvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$pvals)) {
            pval_field <- cardinalNumericField(ns(paste0("p_value", index)), ns(paste0("p_value_card", index)), value = default_pval, label = "p value", min = 0, 
                max = 1, step = 0.01)
            
        } else {
            pval_field <- hiddenInput(ns(paste0("p_value", index)), values = "NULL")
            pval_field <- hiddenInput(ns(paste0("p_value_card", index)), values = "NULL")
        }
        contrast_field_set <- c(contrast_field_set, list(pval_field))
        
        # q value field
        
        if ("qvals" %in% names(ese@contrast_stats[[assay]]) && !is.null(ese@contrast_stats[[assay]]$qvals)) {
            qval_field <- cardinalNumericField(ns(paste0("q_value", index)), ns(paste0("q_value_card", index)), value = default_qval, label = "q value", min = 0, 
                max = 1, step = 0.01)
        } else {
            qval_field <- hiddenInput(ns(paste0("q_value", index)), values = "NULL")
            qval_field <- hiddenInput(ns(paste0("q_value_card", index)), values = "NULL")
        }
        contrast_field_set <- c(contrast_field_set, list(qval_field))
        
        fold_change_field <- cardinalNumericField(ns(paste0("fold_change", index)), ns(paste0("fold_change_card", index)), value = default_foldchange, label = "fold change", 
            cardinality = ">= or <= -", step = 0.5)
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
            selected = contrast_numbers
            if (!select_all) {
                selected = contrast_numbers[1]
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
    
    if (length(unique(table$Variable)) > 1 | length(unique(table[["Condition 1"]])) > 1 | length(unique(table[["Condition 2"]])) > 1) {
        stop("Table represents multiple contrasts, it cannot be simplified.")
    }
    
    colnames(table)[4:5] <- c(as.character(table[1, "Condition 1"]), as.character(table[1, "Condition 2"]))
    table[, 4:ncol(table)]
} 
