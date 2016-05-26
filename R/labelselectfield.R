labelselectfieldInput <- function(id, max_items = 1) {
    
    ns <- NS(id)
    
    selectizeInput(ns("label"), "Label", choices = NULL, options = list(placeholder = "Type a label", maxItems = max_items, addPrecedence = TRUE))
}

labelselectfield <- function(input, output, session, eselist, getExperiment = NULL, labels_from_all_experiments = FALSE, url_field = "label") {
    
    # This module will normally be initialised with a reactive that returns the currently selected experiment, whose metadata will be used for
    # gene symbols etc. But if that reactive is not present, we can use values from ALL experiments. In the latter case the field will be more
    # static, in the former it will depend on the value of any experiment-selecting field.
    
    getExperiments <- reactive({
        if (is.null(getExperiment)) {
            eselist
        } else {
            list(getExperiment())
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
            mcols(ese)[[ese@labelfield]]
        })
        
        sort(Reduce(union, label_lists))
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list
    
    observe({
        updateSelectizeInput(session, "label", choices = getValidLabels(), server = TRUE)
    })
    
    # Get the value of the gene label field
    
    getSelectedLabels <- reactive({
        validate(need(!is.null(input$label) && input$label != "", FALSE))
        input$label
    })
    
    # Get the row or rows of the data that correspond to this symbol
    
    getSelectedIds <- reactive({
        labels <- getSelectedLabels()
        exps <- getExperiments()
        
        id_lists <- lapply(exps, function(ese) {
            rownames(ese)[which(mcols(ese)[[ese@labelfield]] %in% labels)]
        })
        
        sort(Reduce(union, id_lists))
    })
    
    # A reactive for updating the label input field
    
    updateLabelField <- reactive({
        query <- parseQueryString(session$clientData$url_search)
        updateSelectizeInput(session, "label", selected = query[[url_field]], choices = getValidLabels(), server = TRUE)
    })
    
    list(getSelectedLabels = getSelectedLabels, getValidLabels = getValidLabels, getSelectedIds = getSelectedIds, updateLabelField = updateLabelField)
} 
