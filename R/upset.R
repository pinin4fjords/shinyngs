#' The input function of the upset module
#' 
#' This module illustrates the intersection of differential sets using the 
#' \code{\link[UpSetR]{upset}} tool of Lex, Gehlenborg et al.
#' 
#' This function provides the form elements to control the display
#' 
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#' 
#' @references 
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html} 
#' 
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'   
#' @examples 
#' upsetInput('myid', eselist)

upsetInput <- function(id, eselist){
  ns <- NS(id)
  
  upset_fields <- list(
    uiOutput(ns('nsets')),
    sliderInput(ns('nintersects'), label = 'Number of intersections', min = 2, max = 20, step = 1, value = 10),
    selectInput(ns('group_by'), label = 'Group by', choices = c('degree', 'sets'), selected = 'degree')
  )
  
  fieldSets(ns("fieldset"), list(intersections = upset_fields, expression = selectmatrixInput(ns("upset"), eselist), contrasts = contrastsInput(ns('upset')), export = plotdownloadInput(ns("upset"), "UpSet Plot") ))
}

#' The output function of the clustering module
#' 
#' This module illustrates the intersection of differential sets using the 
#' \code{\link[UpSetR]{upset}} tool of Lex, Gehlenborg et al.
#' 
#' This function provides the form elements to control the display
#' 
#' This provides actual plot element for display by applications
#' 
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @references 
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html} 
#' 
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#' 
#' @examples
#' upsetOutput('myid', eselist)

upsetOutput <- function(id, eselist){
  ns <- NS(id)
  
  list(
    h3('Intersection of differential sets'),
    plotOutput(ns('upset'), height = "600px"),
    uiOutput(ns('differential_parameters')),
    simpletableOutput(ns('upset'))
  )
}

#' The server function of the upstart module
#' 
#' This module illustrates the intersection of differential sets using the 
#' \code{\link[UpSetR]{upset}} tool of Lex, Gehlenborg et al.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @keywords shiny
#'   
#' @references 
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html} 
#' 
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}
#'   
#' @examples
#' callModule(upstart, 'myid', eselist)

upset <- function(input, output, session, eselist) {
  
  ns <- session$ns
  
  # Call the selectmatrix module and unpack the reactives it sends back
  
  unpack.list(
    callModule(
      selectmatrix,
      "upset",
      eselist,
      var_n = 1000,
      select_samples = FALSE,
      select_genes = TRUE,
      provide_all_genes = TRUE,
      select_meta = FALSE
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
  
  #############################################################################
  #
  # Render dynamic fields
  #
  #############################################################################
  
  output$nsets <- renderUI({
    
    valid_sets <- getValidSets()
    sliderInput(
      ns('nsets'),
      label = 'Number of sets',
      min = 2,
      max = length(valid_sets),
      step = 1,
      value = length(valid_sets)
    )
  })
  
  output$differential_parameters <- renderUI({
    experiment_id <- getExperimentId()
    assay <- getAssay()
    fc_min <- fcMin()
    qval_max <- qvalMax()
    
    message <- paste0('Summary of all ', experiment_id, 's in assay "', assay, '" differential by a fold change threshold of ', fc_min)
    
    if (qval_max < 1){
      message <- paste0(message, ' and a q value threshold of ', qval_max)
    }
    h4(message)
  })
  
  #############################################################################
  #
  # Form accessors
  #
  #############################################################################
  
  # Accessor for the nsets parameter
  
  getNsets <- reactive({
    validate(need(! is.null(input$nsets), 'Waiting for nsets'))  
    input$nsets
  })
  
  # Accessor for the nintersections parameter
  
  getNintersections <- reactive({
    validate(need(! is.null(input$nintersects), 'Waiting for nintersects'))  
    input$nintersects
  })
  
  # Accessor for the groupby parameter 
  
  getGroupby <- reactive({
    validate(need(! is.null(input$group_by), 'Waiting for group_by'))
    input$group_by
  })
  
  #############################################################################
  #
  # The business end- derive sets and pass for intersection
  #
  #############################################################################
  
  # Look at the contrasts and remove any contrast with no differential features
  
  getValidSets <- reactive({
    fcts <- filteredContrastsTables()
    names(fcts) <- getSafeSelectedContrastNames()
    fcts <- fcts[unlist(lapply(fcts, function(x) nrow(x) > 0))]
    lapply(fcts, rownames)
  })
  
  output$upset <- renderPlot({
      makeUpsetPlot(getValidSets(), nsets = getNsets(), nintersects = getNintersections(), group_by =  getGroupby())
  })
  
  makeUpsetPlotForDownload <- reactive({
    makeUpsetPlot(getValidSets(), nsets = getNsets(), nintersects = getNintersections(), group_by =  getGroupby())
  })
  
  
  callModule(simpletable, "upset", downloadMatrix = makeDifferentialSetSummary, displayMatrix = makeDifferentialSetSummary, filter = "none", 
             filename = "differential_summary", rownames = FALSE)
  
  # Call to plotdownload module
  
  callModule(plotdownload, "upset", makePlot = makeUpsetPlotForDownload, filename = "upset.png", plotHeight = 800, plotWidth = 1200)
  
  
}

#' Make an UpSet plot using the \code{\link[UpSetR]{upset}} tool of Lex, Gehlenborg et al.
#' 
#' Right now this function is a bit superfluous, passing parameters directly on
#' to upset.
#'
#' @param list_input Feature sets as a list of vectors.
#' @param text.scale Scaling factor for text elements
#' @param nsets Number of sets to use
#' @param nintersects Number of intersections to display
#' @param empty.intersections Show empty intersections?
#' @param point.size Point size for matrix
#' @param group_by Group by 'degree' or 'sets'
#'
#' @import UpSetR
#' 
#' @export
#' 
#' @references 
#' Lex and Gehlenborg (2014). Points of view: Sets and intersections. <em>Nature Methods</em> 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html} 
#' 
#' Gehlenborg N (2016). <em>UpSetR: A More Scalable Alternative to Venn and Euler Diagrams for Visualizing Intersecting Sets</em>. R package version 1.3.0, \url{https://CRAN.R-project.org/package=UpSetR}

makeUpsetPlot <- function(list_input, nsets = 10, nintersects = 20, empty.intersections = FALSE, text.scale = 1.8, point.size = 3, group_by = 'degree'){
  UpSetR::upset(
    fromList(list_input),
    nsets = nsets,
    nintersects = nintersects,
    order.by = "freq",
    empty.intersections = empty.intersections,
    text.scale = text.scale,
    group.by = group_by,
    point.size = point.size
  )
}
