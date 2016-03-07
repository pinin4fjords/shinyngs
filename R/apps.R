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

prepareApp <- function(type, ses, title = "ShinyNGS application", ...) {
    
    # Convert to a list if a SummarizedExperiment supplied
    
    if (!is.list(ses)) {
        ses <- list(expression = ses)
    }
    
    # Group by any factor variable by default
    
    # ses <- lapply(ses, function(se){ if (! 'group_vars' %in% colnames(metadata(se))){ metadata(se)$group_vars <- colnames(colData(se))[unlist(lapply(names(colData(se)), function(var)
    # is.factor(colData(se)[[var]])))] } if (! 'default_groupvar' %in% colnames(metadata(se))){ metadata(se)$default_groupvar <- metadata(se)$group_vars[1] } se })
    
    args <- list(...)
    
    if (type == "rnaseq") {
        
        app <- list(ui = rnaseqInput("rnaseq", ses, title), server = function(input, output, session) {
            callModule(rnaseq, "rnaseq", ses)
        })
        
    } else {
        app <- simpleApp(ses, type, title)
    }
    
    # if (type == 'simpletable') { ui <- fluidPage(shinyjs::useShinyjs(), navbarPage(id = 'pages', title = 'A simple table page:', tabPanel('Home', simpletableLayout(se, params)))) server <-
    # function(input, output, session) { callModule(simpletable, 'simpletable', data.frame(colData(se))) } }
    
    # return(list(ui = ui, server = server))
    
    app
}

#' Produce a simple app with controls and layout for a single module, in a
#' shiny \code{sideBarLayout()}. 
#' 
#' Internal function called by prepareApp() to make simple single-function 
#' apps.
#'
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#' @param module Character string specifying the module to use
#' @param title An optional title string to use with the module name
#'
#' @keywords shiny
#'
#' @examples
#' simpleApp(ses, 'heatmap', 'My study name')

simpleApp <- function(ses, module = NULL, title = NULL, ui_only = FALSE) {
    
    inputFunc <- get(paste0(module, "Input"))
    outputFunc <- get(paste0(module, "Output"))
    
    moduletitle <- prettifyVariablename(module)
    
    if (!is.null(title)) {
        title <- paste(title, moduletitle, sep = ": ")
    } else {
        title <- moduletitle
    }
    
    if (!is.null(module)) {
        
        ui <- fluidPage(shinyjs::useShinyjs(), navbarPage(id = "pages", title = title, windowTitle = title, tabPanel(prettifyVariablename(module), sidebarLayout(sidebarPanel(inputFunc(module, 
            ses)), mainPanel(outputFunc(module))))))
        
        if (ui_only) {
            server <- function(input, output, session) {
            }
        } else {
            server = function(input, output, session) {
                callModule(get(module), module, ses)
            }
        }
        list(ui = ui, server = server)
    }
}

#' Produce the controls and output for a datatable, in a shiny
#' \code{sideBarLayout()}.
#' 
#' Just an abstraction to make prepareApp more concise 
#'
#' @param params A list object of parameters
#'
#' @keywords shiny
#'
#' @examples
#' tabPanel('Home', simpletableLayout(se, params))))

simpletableLayout <- function(se, params) {
    sidebarLayout(sidebarPanel(simpletableInput("simpletable", description = "These are the samples involved in this study, and their associated variables. Contrasts for differential expression are built from these variables"), 
        width = 3), mainPanel(simpletableOutput("simpletable", tabletitle = "Experimental variables"), width = 9))
} 
