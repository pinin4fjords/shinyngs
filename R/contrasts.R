#' The input function of the contrasts module
#' 
#' This module provides the form elements to control contrasts used in e.g. 
#' differential expression panels.
#'
#' @param id Submodule namespace
#' @param default_max_q default value for the q value filter
#' @param allow_filtering Provide the filtering fields? Can be disabled to
#' produce unfiltered contrasts tables.
#' @param summarise Provide summarisation controls? Allow user to control how 
#'   how values are summarised per group. Disabling this disables 
#'   summarisation, which may be the desired result for modules that just need
#'   to use the contrasts drop-down. 
#' @param dyanamic_filters Logical indicating whether the user should be able 
#'   to add progressive filters.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' contrastsInput('test')

contrastsInput <- function(id, allow_filtering = TRUE, summarise = TRUE, dynamic_filters = TRUE) {
    
    ns <- NS(id)
    
    inputs <- list()
    
    if (allow_filtering) {
        inputs <- pushToList(inputs, checkboxInput(ns("filterRows"), "Filter rows", TRUE))
    } else {
        inputs <- pushToList(inputs, shinyjs::hidden(checkboxInput(ns("filterRows"), "Filter rows", FALSE)))
    }
    
    # Contrasts filters added by the observeEvent() in the server function. If no dynamic filters are to be provided, then all we
    # need to do is provide a placeholder and a single set of filters will be provided when the page loads. If dynamic filters ARE
    # to be provided, then some other buttons etc are required.
    
    contrast_filters <- list(tags$div(id = ns("contrasts-placeholder")))
    
    if (dynamic_filters) {
        contrast_filters <- c(list(helpText("Build up a complex query by adding filters below"), hr()), contrast_filters, list(hr(), 
            uiOutput(ns('combine_operator')),
                                                                                                                                actionButton(ns("insertBtn"), "Add"), HTML("&nbsp;"), actionButton(ns("removeBtn"), "Remove")))
    } else {
        contrast_filters <- pushToList(contrast_filters, uiOutput(ns('combine_operator')))
    }
    
    inputs <- pushToList(inputs, conditionalPanel(condition = paste0("input['", ns("filterRows"), "'] == true "), contrast_filters))
    
    
    if (summarise) {
        inputs <- pushToList(inputs, summarisematrixInput(ns("contrasts"), allow_none = FALSE, select_summary_type = FALSE))
    }
    
    inputs
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
 

contrastsOutput <- function(id){
  ns <- NS(id)
  uiOutput(ns('summary'))
}


#' The server function of the contrasts module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param getExperiment Reactive for getting the selected experiment. Probably 
#' get this from the \code{selectmatrix} module
#' @param selectMatrix Reactive for generating a matrix to do comparisons with.
#' Probably returned by a call to the \code{\link{selectmatrix}} module.
#' @param selectColData Reactive returning column metadata for the matrix 
#' provided by selectMatrix. Probably returned by a call to the 
#' \code{\link{selectmatrix}} module.
#' @param getAssay Reactive for fetching the current assay. 
#' @param multiple Allow selection of multiple contrasts?
#' @param show_controls Show the controls for contrast selection? 
#' @param default_min_foldchange default value for the fold change filter
#'
#' @keywords shiny
#' 
#' @import data.table
#' 
#' @examples
#' callModule(contrasts, 'differential', getExperiment = getExperiment, selectMatrix = selectMatrix, getAssay = getAssay, multiple = TRUE)

contrasts <- function(input, output, session, eselist, selectmatrix_reactives = list(), multiple = FALSE, show_controls = TRUE, 
    default_min_foldchange = 2) {
    
    ns <- session$ns
    
    unpack.list(selectmatrix_reactives)
    
    getSummaryType <- callModule(summarisematrix, "contrasts")
    
    ###########################################################################
    #
    # Rendering the contrast control filter sets
    #
    ###########################################################################
    
    inserted <- c() # Stores the list of inserted filter sets
    filterset_values <- list() # Stores the list of values in the filter set
    makeReactiveBinding("filterset_values") # Make the stored values reactive
    
    # This observer adds a set of filters on the first page load and when the
    # assocaited button is clicked.
    
    observeEvent({
        getSummaries()
        input$insertBtn
    }, {
        
        ese <- getExperiment()
        contrasts <- getAllContrasts()
        summaries <- getSummaries()
        contrast_numbers <- getAllContrastsNumbers()
        assay <- getAssay()
        
        # btn keeps track of how many filter sets have been added
        
        btn <- length(inserted)
        
        # Call makeContrastFilterSet() to generate a set of filters, and add to
        # the UI with insertUI()
        
        insertUI(selector = paste0("#", ns("contrasts-placeholder")), where = "beforeEnd", ui = makeContrastFilterSet(ns, ese, 
            assay, summaries, contrasts, contrast_numbers, multiple = multiple, show_controls = show_controls, default_min_foldchange = default_min_foldchange, 
            filter_rows = getFilterRows(), index = btn))
        
        # Record the ID of the added filter set
        
        inserted <<- c(inserted, paste0("contrast", btn))
        
        # Now add observers for each new element generated by 
        # makeControlFilterSet(). When they fire, these observers will add the 
        # filter values to the reactive filterset_values. When this is 
        # referenced (by e.g. fcMin()), the dependency chain is established 
        # such that outputs are refreshed when the dynamically added fields are 
        # altered.
        
        filterId <- paste0("filter", btn)
        
        lapply(c("contrasts", "fcMin", "qvalMax", "pvalMax"), function(field) {
            filter_field_id <- paste0(field, btn)
            observeEvent(input[[filter_field_id]], {
                if (is.null(filterset_values[[filterId]])) {
                  filterset_values[[filterId]] <<- list()
                }
                filterset_values[[filterId]][[field]] <<- input[[filter_field_id]]
            }, ignoreNULL = FALSE)
        })
    }, ignoreNULL = FALSE)
    
    # This observer removes a filter set when the 'remove' button is clicked.
    # This removes both the UI element and its stored values in 
    # filterset_values
    
    observeEvent(input$removeBtn, {
        if (length(inserted) > 1) {
            removeUI(selector = paste0("#", inserted[length(inserted)]))
            inserted <<- inserted[-length(inserted)]
            filterset_values[[length(filterset_values)]] <<- NULL
        }
    })
    
    # The combine_operator field is only necessary with more than one filter set
    
    output$combine_operator <- renderUI({
        scn <- getSelectedContrastNumbers()
        if (length(scn) > 1){
          inlineField(selectInput(ns("combine_operator"), NULL, c(and = "intersect", or = "union")), label = "Combine using", 8)
        }else{
          hiddenInput(ns("combine_operator"), "intersect")
        }
    })
    
    ###########################################################################
    #
    # Accessors for form values
    #
    ###########################################################################
    
    # Get current value of field which determines if the table should be filtered at all.
    
    getFilterRows <- reactive({
      as.logical(input$filterRows)
    })
    
    # Get the indices of the currently selected contrasts for each filter set
    # by querying filterset_values.
    
    getSelectedContrastNumbers <- reactive({
      lapply(filterset_values, function(x) x$contrasts)
    })
    
    # Fetch the values from all the fold change filters
    
    fcMin <- reactive({
      unlist(lapply(filterset_values, function(x) x$fcMin))
    })
    
    # Get current value of the q value filter
    
    qvalMax <- reactive({
      unlist(lapply(filterset_values, function(x) x$qvalMax))
    })
    
    # Get current value of the p value filter
    
    pvalMax <- reactive({
      unlist(lapply(filterset_values, function(x) x$pvalMax))
    })
    
    # Get method for combining filters
    
    getFilterSetCombinationOperator <- reactive({
      input$combine_operator
    })
    
    ###########################################################################
    #
    # Generate summaries and contrast stats for all contrasts and all rows. 
    # Then the data is handy for subsetting by other functions. 
    #
    # NOTE: this uses ALL rows in the input ExploratorySummarizedExperiment. 
    # The rows in the matrix returned by selectMatrix() will but a subset, but
    # any modifications to the rows used will not necessitate a re-calculation
    # of these basic stats.
    #
    ###########################################################################
    
    # Generate the summary statistic (probably mean) for column groups as defined by the possible contrasts. Other functions can
    # then pick from this output and calculate fold changes etc.
    
    getSummaries <- reactive({
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
    
    # Main function for returning the table of contrast information. Means, fold changes calculated on the fly, p/q values must be
    # supplied in a 'tests' slot of the ExploratorySummarizedExperiment. Make a summary table for every contrast. This data can
    # then be re-used when processing filter sets.
    
    contrastsTables <- reactive({
      matrix <- selectMatrix()
      
      ese <- getExperiment()
      summaries <- getSummaries()
      contrasts <- getAllContrasts()
      assay <- getAssay()
      
      # There can be a mismatch between the conrasts and summaries as we adjust the input matrix. Wait for updates to finish before
      # making the table.
      
      # validate(need(all(unlist(lapply(selected_contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))), 'Matching
      # summaries and contrasts'))
      
      withProgress(message = "Calculating contrast tables", value = 0, {
        
        contrast_tables <- lapply(names(contrasts), function(c) {
          
          cont <- contrasts[[c]]
          
          smry1 <- summaries[[cont[1]]][, cont[2], drop = FALSE]
          smry2 <- summaries[[cont[1]]][, cont[3], drop = FALSE]
          
          ct <- data.frame(cont[1], cont[2], cont[3], round(smry1, 2), round(smry2, 2), round(foldChange(smry1, smry2), 
                                                                                              2))
          names(ct) <- c("Variable", "Condition 1", "Condition 2", cont[2], cont[3], "Fold change")
          
          if (length(ese@tests) > 0 && assay %in% names(ese@tests)) {
            pvals <- ese@tests[[assay]]$pvals
            qvals <- ese@tests[[assay]]$qvals
            
            ct[["p value"]] <- signif(pvals[match(rownames(ct), rownames(pvals)), as.numeric(c)], 5)
            ct[["q value"]] <- signif(qvals[match(rownames(ct), rownames(qvals)), as.numeric(c)], 5)
            
          } else {
            ct[["p value"]] <- NA
            ct[["q value"]] <- NA
          }
          ct
          
        })
      })
      
      names(contrast_tables) <- getAllContrastsNumbers()
      contrast_tables
    })
    
    ###########################################################################
    #
    # Subsetting using the rows in the input matrix. This does NOT involve the
    # filters from this module, but simply subsets the base data to the 
    # rows pertinent to the input matrix.
    #
    ###########################################################################
    
    # Select out the rows from the summaries corresponding to the rows of the selected matrix
    
    getSummariesWithSelectedRows <- reactive({
      summaries <- getSummaries()
      matrix <- selectMatrix()
      
      lapply(summaries, function(s) s[rownames(matrix), ])
    })
    
    # Get contrasts tables with rows reflecting the input matrix
    
    contrastsTablesWithSelectedRows <- reactive({
      contrast_tables <- contrastsTables()
      matrix <- selectMatrix()
      
      lapply(contrast_tables, function(ct) {
        ct[rownames(matrix), ]
      })
    })
    
    ###########################################################################
    #
    # Contrast naming
    #
    ###########################################################################
    
    # Make names for the contrasts
    
    makeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(x) paste(prettifyVariablename(x[1]), paste(x[3], x[2], sep = " vs "), sep = ": "))
    })
    
    # Make safe set of names to be used where spaces etc not allowed
    
    makeSafeContrastNames <- reactive({
        contrasts <- getAllContrasts()
        
        lapply(contrasts, function(x) paste(ucfirst(prettifyVariablename(x[1])), paste(ucfirst(x[3]), ucfirst(x[2]), sep = "VS"), 
            sep = "."))
    })
    
    ###########################################################################
    #
    # Subset contrast-related variables according to filters
    #
    ###########################################################################
    
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
        contrast_names[getSelectedContrastNumbers()]
    })
    
    # The same, but with safe names that won't get mangled by plotting etc
    
    getSafeSelectedContrastNames <- reactive({
        contrast_names <- makeSafeContrastNames()
        contrast_names[getSelectedContrastNumbers()]
    })
    
    # Get samples for currently selected contrast
    
    getSelectedContrastSamples <- reactive({
        contrast_samples <- getContrastSamples()
        selected_contrasts <- getSelectedContrastNumbers()
        contrast_samples[[selected_contrasts]]
    })

    ###########################################################################
    #
    # Process data in response to contrasts filters
    #
    ###########################################################################

    # Filter contrasts tables down to the contrasts of interest
    
    selectedContrastsTables <- reactive({
        
        selected_contrasts <- getSelectedContrastNumbers()
        contrast_tables <- contrastsTablesWithSelectedRows()
        
        # Selected contrasts is a list, one for each filter set. Each one can have multiple contrasts
        
        withProgress(message = "Filtering to specified features", value = 0, {
            
            lapply(selected_contrasts, function(scs_set) {
                lapply(scs_set, function(s) {
                  contrast_tables[[s]]
                })
            })
            
        })
    })
    
    # Apply user filters to results of contrastsTables(). Called on first page load and on subsequent clicks of 'Apply'.
    
    filteredContrastsTables <- reactive({
        
        selected_contrasts_tables <- selectedContrastsTables()
        
        if (getFilterRows()) {
            ese <- getExperiment()
            assay <- getAssay()
            
            fc_min <- fcMin()
            pval_max <- pvalMax()
            qval_max <- qvalMax()
            
            withProgress(message = "Applying filters", value = 0, {
                
                fcts <- lapply(1:length(selected_contrasts_tables), function(i) {
                  sct <- selected_contrasts_tables[[i]]
                  
                  lapply(sct, function(s) {
                    filter <- abs(s[["Fold change"]]) >= fc_min[i]
                    
                    if (length(ese@tests) > 0 && assay %in% names(ese@tests)) {
                      filter <- filter & s[["p value"]] <= pval_max[i] & s[["q value"]] <= qval_max[i]
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
    
    selectFinalFeatures <- reactive({
        filtered_contrasts_tables <- filteredContrastsTables()
        
        withProgress(message = "Selecting final feature set", value = 0, {
            Reduce(get(getFilterSetCombinationOperator()), lapply(filtered_contrasts_tables, function(fcts) {
                Reduce(intersect, lapply(fcts, function(fct) {
                  rownames(fct)
                }))
            }))
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
            final_contrasts_table <- unique(data.table::rbindlist(lapply(filtered_contrast_tables, function(fcts) {
                data.table::rbindlist(lapply(fcts, function(fct) {
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
        contrasts <- getSelectedContrasts()
        
        summary <- data.frame(do.call(rbind, contrasts))
        colnames(summary) <- c("Variable", "group 1", "group 2")
        summary[["Set size"]] <- unlist(lapply(fcts, nrow))
        summary
    })
    
    ###########################################################################
    #
    # Tell the user something about the query and its results
    #
    ###########################################################################
    
    output$summary <- renderUI({
      lct <-labelledContrastsTable()
      
      list(
        tags$br(),
        fluidRow(
          column(6, wellPanel(h4('Query Summary'), htmlOutput(ns('query_summary')))),
          column(6, wellPanel(h4('Results summary'), htmlOutput(ns('results_summary'))))
        )
      )
    })
    
    output$query_summary <- renderText({
      contrast_names <- makeContrastNames()

        fvs <- lapply(filterset_values, function(fv){
            fv$contrasts <- contrast_names[fv$contrasts]
            fv
        })
      
        paste(unlist(lapply(fvs, function(x){
            paste('<p>', paste(unlist(lapply(names(x[-1]), function(y){
                paste(y, '=', x[[y]])
            })), collapse = ' AND '), 'in <i>', x[[1]], '</i></p>')
        })), collapse = '<p><b>AND</b></p>')
    })
    
    ###########################################################################
    #
    # Return the reactive that allow other modules to interact with this one
    #
    ###########################################################################
    
    list(fcMin = fcMin, qvalMax = qvalMax, getAllContrasts = getAllContrasts, getSelectedContrasts = getSelectedContrasts, getSelectedContrastNumbers = getSelectedContrastNumbers, 
        getSelectedContrastNames = getSelectedContrastNames, getSafeSelectedContrastNames = getSafeSelectedContrastNames, getContrastSamples = getContrastSamples, 
        getSelectedContrastSamples = getSelectedContrastSamples, contrastsTables = contrastsTables, filteredContrastsTables = filteredContrastsTables, 
        labelledContrastsTable = labelledContrastsTable, linkedLabelledContrastsTable = linkedLabelledContrastsTable, makeDifferentialSetSummary = makeDifferentialSetSummary)
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
#' @param summaries Summaries derived from \code{\link{summarizematrix}}, will
#' be used to select valid contrasts.
#' @param contrasts A list of lists specifying contrasts. 
#' @param contrast_numbers A named vector of indices corresponding to 
#' \code{contrasts}.
#' @param multiple Allow multiple contrasts to be selected? Passed to 
#' \code{\link{makeContrastControl}}.
#' @param show_controls Show controls? Setting to false will cause them to be 
#' hidden.
#' @param default_min_foldchange Default value for the fold change field 
#' @param index Index. Will be used to differentiate mutiple copies of the
#' field set.
#' @param filter_rows Use fold change and p value etc to filter values? 
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 

makeContrastFilterSet <- function(ns, ese, assay, summaries, contrasts, contrast_numbers, multiple, show_controls, default_min_foldchange = default_min_foldchange, 
    index = "", filter_rows = TRUE) {
    
    contrast_field_set <- list(makeContrastControl(ns(paste0("contrasts", index)), contrasts, contrast_numbers, summaries, multiple = multiple, 
        show_controls = show_controls))
    
    if (filter_rows) {
        
        # p value field
        
        if ("pvals" %in% names(ese@tests[[assay]]) && !is.null(ese@tests[[assay]]$pvals)) {
            pval_field <- inlineField(numericInput(ns(paste0("pvalMax", index)), NULL, value = 0.05), label = "Maximum p value")
        } else {
            pval_field <- hiddenInput(ns(paste0("pvalMax", index)), 1)
        }
        
        # q value field
        
        if ("qvals" %in% names(ese@tests[[assay]]) && !is.null(ese@tests[[assay]]$qvals)) {
            qval_field <- inlineField(numericInput(ns(paste0("qvalMax", index)), NULL, value = 0.1), label = "Maximum q value")
        } else {
            qval_field <- hiddenInput(ns(paste0("qvalMax", index)), 1)
        }
        
        # Create a set of all the contrast fields
        
        contrast_field_set <- c(contrast_field_set, list(inlineField(numericInput(ns(paste0("fcMin", index)), NULL, value = default_min_foldchange), 
            label = "Minimum absolute fold change"), pval_field, qval_field))
    }
    
    tags$fieldset(id = paste0("contrast", index), contrast_field_set, class = "shinyngs-contrast")
    
}

#' Make a select field for picking one or more contrasts
#'
#' @param id An id to apply to form elements
#' @param contrasts A list of lists specifying contrasts. 
#' @param contrast_numbers A named vector of indices corresponding to 
#' \code{contrasts}.
#' @param summaries Summaries derived from \code{\link{summarizematrix}}, will
#' be used to select valid contrasts.
#' @param multiple Allow multiple contrasts to be selected?
#' @param show_controls Show controls? Setting to false will cause them to be 
#' hidden.
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 

makeContrastControl <- function(id, contrasts, contrast_numbers, summaries, multiple = FALSE, show_controls = TRUE) {
    
    contrast_numbers <- contrast_numbers[unlist(lapply(contrasts, function(x) all(x[-1] %in% colnames(summaries[[x[1]]]))))]
    
    if (!is.null(contrast_numbers)) {
        
        if (multiple) {
            cont_control <- checkboxGroupInput(id, "Contrast(s):", contrast_numbers, selected = contrast_numbers)
        } else {
            cont_control <- selectInput(id, "Contrast(s):", contrast_numbers)
        }
        
        if (!show_controls) {
            cont_control <- shinyjs::hidden(cont_control)
        }
        cont_control
    }
} 
