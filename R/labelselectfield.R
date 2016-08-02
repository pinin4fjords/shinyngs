labelselectfieldInput <- function(id, max_items = 1, id_selection = FALSE) {
    
    ns <- NS(id)
    
    filters <- list(
      uiOutput(ns('metaFields')),
      uiOutput(ns('metaValue'))
    )
    
    if (id_selection){
      filters <- pushToList(filters, uiOutput(ns('labelIds'))) 
    }
    
    filters
}

labelselectfield <- function(input, output, session, eselist, getExperiment = NULL, labels_from_all_experiments = FALSE, url_field = "label", max_items = 1, field_selection = FALSE, id_selection = FALSE) {
    
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
    
    # What field to use? 
    
    getSelectedMetaField <- reactive({
        validate(need(input$metaField, 'FALSE'))
        mf <- input$metaField
        mf
    })
    
    output$metaFields <- renderUI({
        ns <- session$ns
        
        ese <- getExperiment()
        
        if (field_selection){
          metaFields <- getMetaFields()
          selectInput(ns('metaField'), label = 'Metadata field', choices = structure(metaFields, names = prettifyVariablename(metaFields)), selected = ese@labelfield)
        }else{
          hiddenInput(ns('metaField'), values = ese@labelfield) 
        }
    })
            
    # Allow the choice of any non-numeric field     
            
    getMetaFields <- reactive({
      ese <- getExperiment()
      names(mcols(ese))[! unlist(lapply(mcols(ese), is.numeric))]
    })
      
    # Set the meta values filter dependent on field
    
    output$metaValue <- renderUI({
      ns <- session$ns
      mf <- getSelectedMetaField()
      selectizeInput(ns("label"), prettifyVariablename(mf), choices = NULL, options = list(placeholder = "Type a value or scroll", maxItems = max_items, addPrecedence = TRUE))
    })
    
    # Allow selection from the ids pertaining to a given label
    
    output$labelIds <- renderUI({
        ns <- session$ns
        
        ids <- getAssociatedIds()
        
        if (length(ids) == 1){
          hiddenInput(ns('ids'), ids) 
        }else{
          checkboxGroupInput(ns('ids'), label = 'Associated IDs', choices = getAssociatedIds(), selected = getAssociatedIds())
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
            mcols(ese)[[mf]]
        })
        
        sort(unique(Reduce(union, label_lists)))
    })
    
    # Server-side function for populating the selectize input. Client-side takes too long with the likely size of the list
    
     # observe({
     #     print("updating label field 1")
     #     updateSelectizeInput(session, "label", choices = getValidLabels(), server = TRUE)
     # })
    
    observeEvent(input$metaField, {
        updateSelectizeInput(session, "label", choices = getValidLabels(), server = TRUE)
    })
    
    # Get the value of the gene label field
    
    getSelectedLabels <- reactive({
        mf <- getSelectedMetaField()
        validate(need(!is.null(input$label) && input$label != "", FALSE))
        input$label
    })
    
    # Get the row or rows of the data that correspond to the input metadata
    
    getSelectedIds <- reactive({
      if (id_selection){
        validate(need(length(input$ids) > 0, 'Waiting for ID list'))
        input$ids
      }else{
        getAssociatedIds()
      }
    })
    
    getAssociatedIds <- reactive({
        labels <- getSelectedLabels()
        exps <- getExperiments()
        mf <- getSelectedMetaField()
        
        id_lists <- lapply(exps, function(ese) {
            rownames(ese)[which(mcols(ese)[[mf]] %in% labels)]
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
