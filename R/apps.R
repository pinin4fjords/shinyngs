#' Make UI and server functions for apps based on supplied data
#'
#' Draws on various components (heatmaps, tables etc) to produce the UI and server
#' components of a variety of shiny apps, based on the type and data specified.
#'
#'
#' @param type A string specifying the type of shiny app required (options: heatmap)
#' @param se A SummarizedExperiment object containing assay data (expression, 
#' counts...), sample data and annotation data for the rows.
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
#'  transcriptfield = 'ensembl_gene_id', 
#'  entrezgenefield = 'entrezgene',
#'  genefield = 'external_gene_name', 
#'  group_vars = c('cell', 'dex', 'albut'), 
#'  default_groupvar = 'albut'
#')
#'  
#'  # Prepare the UI and server parts of the Shiny app
#'  app <- prepareApp('heatmap', se, params)
#'  
#'  # Run the Shiny app
#'  shinyApp(app$ui, app$server)

prepareApp <- function(type, se, params = list()) {
    
    if (type == "heatmap") {
        
        ui <- fluidPage(shinyjs::useShinyjs(), navbarPage(id = "pages", title = "Heatmap builder:", tabPanel("Home", heatmapLayout(se, params))))
        
        server <- function(input, output, session) {
            heatmapModuleCall(se, params)
        }
        
    } else if (type == "pca") {
        ui <- fluidPage(shinyjs::useShinyjs(), navbarPage(id = "pages", title = "Interactive PCA plot:", tabPanel("Home", pcaLayout(se, 
            params))))
        
        server <- function(input, output, session) {
            pcaModuleCall(se, params)
        }
    }
    
    return(list(ui = ui, server = server))
    
}

#' Produce the controls and output for a heatmap using the heatmap module, in a
#' shiny \code{sideBarLayout()}.
#' 
#' Just an abstraction to make prepareApp more concise 
#'
#' @param params A list object of parameters
#'
#' @keywords shiny
#'
#' @examples
#' tabPanel('Home', pcaLayout(se, params))))

heatmapLayout <- function(se, params) {
    sidebarLayout(sidebarPanel(heatmapInput("heatmap", se, params$group_vars, params$default_groupvar)), mainPanel(heatmapOutput("heatmap")))
}

#' Run the call to the heatmap module's server function
#' 
#' Just an abstraction to make prepareApp more concise 
#'
#' @param se A SummarizedExperiment object containing assay data (expression, 
#' counts...), sample data and annotation data for the rows.
#' @param params A list object of parameters
#'
#' @keywords shiny

heatmapModuleCall <- function(se, params) {
    callModule(heatmap, "heatmap", se, params$transcriptfield, params$entrezgenefield, params$genefield, geneset_files = params$geneset_files)
}

#' Produce the controls and output for a 3D PCA plot using the pca module, in a
#' shiny \code{sideBarLayout()}.
#' 
#' Just an abstraction to make prepareApp more concise 
#'
#' @param params A list object of parameters
#'
#' @keywords shiny
#'
#' @examples
#' tabPanel('Home', pcaLayout(se, params))))

pcaLayout <- function(se, params) {
    sidebarLayout(sidebarPanel(pcaInput("pca", se, params$group_vars, params$default_groupvar)), mainPanel(pcaOutput("pca")))
}

#' Run the call to the pca module's server function
#' 
#' Just an abstraction to make prepareApp more concise 
#'
#' @param se A SummarizedExperiment object containing assay data (expression, 
#' counts...), sample data and annotation data for the rows.
#' @param params A list object of parameters
#'
#' @keywords shiny

pcaModuleCall <- function(se, params) {
    callModule(pca, "pca", se, params$transcriptfield, params$entrezgenefield, params$genefield, geneset_files = params$geneset_files)
} 
