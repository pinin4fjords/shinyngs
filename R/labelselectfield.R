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
#' labelselectfieldInput('myid')

labelselectfieldInput <- function(id, max_items = 1, id_selection = FALSE) {
    
    ns <- NS(id)
    
    filters <- list(uiOutput(ns("metaFields")), uiOutput(ns("metaValue")))
    
    if (id_selection) {
        filters <- pushToList(filters, uiOutput(ns("labelIds")))
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
#' @param input Input object
#' @param output Output object
#' @param session Session object
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
#'
#' @examples
#' callModule(labelselectfield, 'myid', eselist)

labelselectfield <- function(input, output, session, eselist, getExperiment = NULL, labels_from_all_experiments = FALSE, url_field = "label", max_items = 1, field_selection = FALSE, 
    id_selection = FALSE, getAssayIds = NULL) {
    
    # This module will normally be initialised with a reactive that returns the currently selected experiment, whose metadata will be used for gene symbols etc.
    # But if that reactive is not present, we can use values from ALL experiments. In the latter case the field will be more static, in the former it will depend
    # on the value of any experiment-selecting field.
    
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
    
    output$metaFields <- renderUI({
        ns <- session$ns
        
        ese <- getExperiment()
        
        if (field_selection) {
            metaFields <- getMetaFields()
            selectInput(ns("metaField"), label = "Metadata field", choices = structure(metaFields, names = prettifyVariablename(metaFields)), selected = ese@labelfield)
        } else {
            
            # 'id' means use the row IDs
            
            mf <- "id"
            if (length(ese@labelfield) == 0) {
                
                # If the idfield slot has been set, use its value instead of 'id'
                
                if (length(ese@idfield) > 0) {
                  mf <- ese@idfield
                }
            } else {
                mf <- ese@labelfield
            }
            hiddenInput(ns("metaField"), values = mf)
        }
    })
    
    # Fetch the meta field from the input
    
    getSelectedMetaField <- reactive({
        validate(need(input$metaField, FALSE))
        mf <- input$metaField
        mf
    })
    
    ### META-FIELD VALUE SELECTION
    
    # Set the meta values filter dependent on field
    
    output$metaValue <- renderUI({
        ns <- session$ns
        mf <- getSelectedMetaField()
        selectizeInput(ns("label"), prettifyVariablename(mf), choices = NULL, options = list(placeholder = "Type a value or scroll", maxItems = max_items, addPrecedence = TRUE))
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list
    
    observeEvent(input$metaField, {
        updateSelectizeInput(session, "label", choices = getValidLabels(), server = TRUE)
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
        mf <- getSelectedMetaField()
        validate(need(!is.null(input$label) && input$label != "", FALSE))
        input$label
    })
    
    ### SELECTION OF IDS FOR METAFIELD VALUES
    
    # Allow selection from the ids pertaining to a given label
    
    output$labelIds <- renderUI({
        ns <- session$ns
        
        ids <- getAssociatedIds()
        
        if (length(ids) == 1) {
            hiddenInput(ns("ids"), ids)
        } else {
            checkboxGroupInput(ns("ids"), label = "Associated IDs", choices = getAssociatedIds(), selected = getAssociatedIds())
        }
    })
    
    # Get the row or rows of the data that correspond to the input metadata
    
    getSelectedIds <- reactive({
        
        # If the user has been allowed to select IDs, fetch the value of the input field. Othewise return all IDs associated with the selected label
        
        if (id_selection) {
            validate(need(length(input$ids) > 0, "Waiting for ID list"))
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
                  
                  if(! is.null(getAssayIds)){
                    ids <- intersect(ids, getAssayIds())
                  }
                  ids
                }
            })
            sort(Reduce(union, id_lists))
        }
    })
    
    # A reactive for updating the label input field
    
    updateLabelField <- reactive({
        query <- parseQueryString(session$clientData$url_search)
        updateSelectizeInput(session, "label", selected = query[[url_field]], choices = getValidLabels(), server = TRUE)
    })
    
    list(getSelectedLabels = getSelectedLabels, getValidLabels = getValidLabels, getSelectedIds = getSelectedIds, updateLabelField = updateLabelField)
}
