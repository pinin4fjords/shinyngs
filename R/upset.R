upsetInput <- function(id, eselist){
  ns <- NS(id)
  
  list(
    selectmatrixInput(ns("upset"), eselist),
    contrastsInput(ns('upset'))
  )
}

upsetOutput <- function(id, eselist){
  ns <- NS(id)
  
  plotOutput(ns('upset'), height = "600px")
   
}

upset <- function(input, output, session, eselist) {
  
  # Call the selectmatrix module and unpack the reactives it sends back
  
  unpack.list(
    callModule(
      selectmatrix,
      "upset",
      eselist,
      var_n = 1000,
      select_samples = FALSE,
      select_genes = TRUE,
      provide_all_genes = TRUE
    )
  )
  
  # Pass the matrix to the contrasts module for processing
  
  unpack.list(
    callModule(
      contrasts,
      "upset",
      eselist = eselist,
      getExperiment = getExperiment,
      selectMatrix = selectMatrix,
      getAssay = getAssay,
      multiple = TRUE,
      getMetafields = getMetafields,
      selectColData = selectColData
    )
  )
  
  prepareUpsetInput <- reactive({
    fcts <- filteredContrastsTables()
    plotdata <- lapply(fcts, rownames)
    names(plotdata) <- getSafeSelectedContrastNames()
    #names(plotdata) <- unlist(lapply(names(plotdata), splitStringToFixedwidthLines, 20))
    #saveRDS(plotdata, file = "/tmp/plotdata.rds")
    plotdata
  })
  
  output$upset <- renderPlot({
      plot_input <- prepareUpsetInput()
      makeUpsetPlot(plot_input)
  })
  
  
  
}

makeUpsetPlot <- function(list_input, text.scale = 1.5){
  UpSetR::upset(fromList(list_input), order.by = "freq", empty.intersections = TRUE, text.scale = text.scale)
}
