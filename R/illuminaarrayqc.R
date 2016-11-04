#' The input function of the illuminaarrayqc module
#' 
#' This module plots control probes from an illumina microarray experiment.
#' 
#' This funcion provides the form elements to control the display
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
#' @examples
#' library(shinyngs)
#' data(zhangneurons)
#' illuminaarrayqcInput('myid', zhangneurons)

illuminaarrayqcInput <- function(id, eselist) {
  
  ns <- NS(id)
  
  expression_filters <- selectmatrixInput(ns("illuminaarrayqc"), eselist[names(eselist) == 'control'])
  
  fieldSets(ns("fieldset"), list(
    'quality_checks' = tags$ul(
      tags$li("cy3_high > cy3_med > cy3_low"), 
      tags$li("low stringency =~ 0"), 
      tags$li("high stringency <= cy3 high"),
      tags$li("housekeeping = high"),
      tags$li("biotin = high"),
      tags$li("negative =~ 0")
    ),
    'table fields' = expression_filters, export = simpletableInput(ns("qctable"))))
}

#' The output function of the illuminaarrayqc module
#' 
#' This module plots control probes from an illumina microarray experiment.
#' 
#' This funcion provides the form elements to control the display
#' 
#' This provides actual plot element for display by applications
#' 
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' illuminaarrayqcOutput('myid')

illuminaarrayqcOutput <- function(id){
  
  ns <- NS(id)
  
  list(
    modalInput(ns("illuminaarrayqc"), "help", "help"), modalOutput(ns("illuminaarrayqc"), "Quality control plot for Illumina microarray data", includeMarkdown(system.file("inlinehelp", "illuminaarrayqc.md", package = packageName()))),
    h3('Illumina microarray QC plot'),
    plotlyOutput(ns('qcplot'), height = 600),
    h4('Table of data'),
    simpletableOutput(ns('qctable'))

  )
}

#' The server function of the illuminaarrayqc module
#' 
#' This module plots control probes from an illumina microarray experiment.
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
#' @examples
#' callModule(illuminaarayqc, 'myid', eselist)

illuminaarrayqc <- function(input, output, session, eselist) {
  
  unpack.list(callModule(selectmatrix, "illuminaarrayqc", eselist[names(eselist) == 'control'], select_genes = FALSE, select_samples = FALSE, select_assay = FALSE))
  
  output$qcplot <- renderPlotly({
    
    ese <- getExperiment()
    experiment <- selectColData()
    control_annotation <- getAnnotation()
    controls <- selectMatrix()
    
    controls_merged <- merge(control_annotation, controls, by.x="Array_Address_Id", by.y="row.names")
    
    qc_groups <- list(
      cy3_low = "phage_lambda_genome:low",
      cy3_med = "phage_lambda_genome:med",
      cy3_high = "phage_lambda_genome:high",
      low_stringency_pm = "phage_lambda_genome:pm",
      low_stringency_mm = "phage_lambda_genome:mm2",
      negative = "permuted_negative",
      biotin = "phage_lambda_genome",
      labeling = "thrB",
      housekeeping = "housekeeping"
    )
    
    plotdata <- reshape2::melt(do.call(cbind, lapply(qc_groups, function(qcg){
      colMeans(controls_merged[grep(paste0(qcg, '($|,)'), controls_merged$Reporter_Group_id), colnames(controls)])
    })))
    
    group_by(plotdata, Var2) %>% 
      plot_ly() %>% 
      add_lines(
        x = ~ Var1, 
        y = ~ value, 
        color = ~Var2, 
        colors = c('red', 'red', 'red', 'orange', 'orange', 'black', 'purple', 'blue', 'green' ),
        linetype = ~Var2,
        linetypes = c('dot', 'dash', 'solid', 'dash', 'solid', 'solid', 'solid', 'solid', 'solid')
      ) %>% layout(xaxis = list(categoryarray = rownames(experiment), categoryorder = "array", title = ''), yaxis = list(title = 'Intensity'), margin = list(b = 200)) %>% config(showLink = TRUE)
  })

  # Render the table and provide for download, using the simpletable module.
  
  callModule(simpletable, "qctable", downloadMatrix = selectLabelledMatrix, displayMatrix = selectLabelledLinkedMatrix, filename = "illumina_array_qc", rownames = FALSE)
  
}
