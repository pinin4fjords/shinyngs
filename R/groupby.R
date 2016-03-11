groupbyInput <- function(id) {
    ns <- NS(id)
    
    uiOutput(ns("groupby"))
}

groupby <- function(input, output, session, getExperiment, group_label = "Group by") {
    getDefaultGroupby <- reactive({
        se <- getExperiment()
        
        if ("default_groupvar" %in% names(metadata(se))) {
            metadata(se)$default_groupvar
        } else {
            metadata(se)$group_vars[1]
        }
    })
    
    
    output$groupby <- renderUI({
        ns <- session$ns
        se <- getExperiment()
        if ("group_vars" %in% names(metadata(se))) {
            selectInput(ns("groupby"), group_label, structure(metadata(se)$group_vars, names = prettifyVariablename(metadata(se)$group_vars)), selected = getDefaultGroupby())
        }
    })
    
    reactive({
        input$groupby
    })
} 
