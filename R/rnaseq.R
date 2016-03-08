#' The input function of the rnaseq module
#' 
#' This provides the form elements to control the RNA-seq display
#'
#' The rnaseq module is a combination of output from many modules (pca, boxplot
#' etc) to form a comprehensive analysis application.
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#' @param title An optional title to display on the app
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' rnaseqInput('rnaseq', ses)

rnaseqInput <- function(id, ses, title = "") {
    ns <- NS(id)
    
    fluidPage(tags$head(tags$style(HTML(".navbar-brand{height: auto;}"))), navbarPage(id = ns("rnaseq"), title = paste0("RNA-seq explorer: ", title), windowTitle = title, tabPanel("Home", sidebarLayout(sidebarPanel(width=3), mainPanel(width=9))), navbarMenu("Sample data", 
        tabPanel("Experiment", sidebarLayout(sidebarPanel(experimenttableInput(ns("experimenttable"), ses), width=3), mainPanel(experimenttableOutput(ns("experimenttable")), width=9)))), navbarMenu("QC/ exploratory", tabPanel("Boxplots", sidebarLayout(sidebarPanel(boxplotInput(ns("boxplot"), 
        ses), width=3), mainPanel(boxplotOutput(ns("boxplot")), width=9))), tabPanel("PCA", sidebarLayout(sidebarPanel(pcaInput(ns("pca"), ses), width=3), mainPanel(pcaOutput(ns("pca")), width=9))), tabPanel("Clustering dendrogram", sidebarLayout(sidebarPanel(dendroInput(ns("dendro"), 
        ses), width=3), mainPanel(dendroOutput(ns("dendro")), width=9)))), navbarMenu("Expression", tabPanel("Heatmap", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap"), ses), width=3), mainPanel(heatmapOutput(ns("heatmap")), width=9))))))
}

#' The server function of the rnaseq module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(rnaseq, 'rnaseq', ses)

rnaseq <- function(input, output, session, ses) {
  
  callModule(experimenttable, "experimenttable", ses)
  callModule(heatmap, "heatmap", ses)
  callModule(pca, "pca", ses)
  callModule(boxplot, "boxplot", ses)
  callModule(dendro, "dendro", ses)

} 
