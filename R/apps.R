#' Make UI and server functions for apps based on supplied data
#'
#' Draws on various components (heatmaps, tables etc) to produce the UI and server
#' components of a variety of shiny apps, based on the type and data specified.
#'
#'
#' @param type A string specifying the type of shiny app required (options:
#'   heatmap)
#' @param eselist An ExploratorySummarizedExperimentList object containing assay
#'   data (expression, counts...), sample data and annotation data for the rows.
#' @param params A list containing data and display options for the Shiny app
#'
#' @return output A list of length 2 containing: the UI and server components
#'
#' @keywords shiny
#'
#' @import shiny
#' @import plotly
#' @export
#'
#' @examples
#' require(airway)
#' library(shinyngs)
#' library(shiny)
#' library(GenomicRanges)
#' 
#' # Get some example data in the form of a StructuredExperiment object
#' data(airway, package='airway')
#' se <- airway
#' 
#' # Use Biomart to retrieve some annotation, and add it to the object
#' library(biomaRt)
#' attributes <- c(
#'   'ensembl_gene_id', # The sort of ID your results are keyed by
#'   'entrezgene', # Will be used mostly for gene set based stuff
#'   'external_gene_name' # Used to annotate gene names on the plot
#')
#'
#'mart <- useMart(biomart = 'ENSEMBL_MART_ENSEMBL', dataset = 'hsapiens_gene_ensembl', host='www.ensembl.org')
#'annotation <- getBM(attributes = attributes, mart = mart)
#'annotation <- annotation[order(annotation$entrezgene),]
#'
#'mcols(se) <- annotation[match(rownames(se), annotation$ensembl_gene_id),]
#'
#'# Specify some display parameters
#'params <- list(
#'  idfield = 'ensembl_gene_id', 
#'  entrezgenefield = 'entrezgene',
#'  labelfield = 'external_gene_name', 
#'  group_vars = c('cell', 'dex', 'albut'), 
#'  default_groupvar = 'albut'
#')
#'  
#'  # Prepare the UI and server parts of the Shiny app
#'  app <- prepareApp('heatmap', se, params)
#'  
#'  # Run the Shiny app
#'  shinyApp(app$ui, app$server)

prepareApp <- function(type, eselist, ui_only = FALSE, ...) {
    
    if (type == "rnaseq") {
        
        app <- list(ui = rnaseqInput("rnaseq", eselist), server = function(input, output, session) {
            callModule(rnaseq, "rnaseq", eselist)
        })
        
    } else {
        app <- simpleApp(eselist, type, ui_only = ui_only, ...)
    }
    
    app
}

#' Produce a simple app with controls and layout for a single module, in a
#' shiny \code{sideBarLayout()}. 
#' 
#' Internal function called by prepareApp() to make simple single-function 
#' apps.
#'
#' @param ses List of ExploratorySummarizedExperiment objects with assay and experimental
#' data
#' @param module Character string specifying the module to use
#'
#' @keywords shiny
#'
#' @examples
#' simpleApp(ses, 'heatmap', 'My study name')

simpleApp <- function(eselist, module = NULL, ui_only = FALSE, ...) {
    
    inputFunc <- get(paste0(module, "Input"))
    outputFunc <- get(paste0(module, "Output"))
    
    moduletitle <- prettifyVariablename(module)
    
    cssfile <- system.file("www", paste0(packageName(), ".css"), package = packageName())
    
    if (!is.null(module)) {
        
        ui <- fluidPage(includeCSS(cssfile), theme = shinythemes::shinytheme("cosmo"), shinyjs::useShinyjs(), navbarPage(id = "pages", title = moduletitle, 
            windowTitle = moduletitle, tabPanel(prettifyVariablename(module), sidebarLayout(sidebarPanel(inputFunc(module, eselist, ...), width = 3), 
                mainPanel(outputFunc(module, ...), width = 9)))))
        
        if (ui_only) {
            server <- function(input, output, session) {
            }
        } else {
            server = function(input, output, session) {
                callModule(get(module), module, eselist, ...)
            }
        }
        list(ui = ui, server = server)
    }
} 
