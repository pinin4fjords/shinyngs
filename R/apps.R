#' Make UI and server functions for Shiny apps based on data supplied as 
#' modfied SummarizedExperiments
#'
#' Draws on various components (heatmaps, tables etc) to produce the UI and server
#' components of a variety of shiny apps, based on the type and data specified,
#' using modularised Shiny components.
#'
#' @param type A string specifying the type of shiny app required. Currently,
#'   'rnaseq' or 'chipseq' produce large multi-panel applications designed to
#'   facilitate analysis of those data types. For any other 'type', the call is
#'   passed to \code{simpleApp()}, which will attempt to build an application
#'   using \code{Input} and \code{Output} functions of the same module.
#' @param eselist An ExploratorySummarizedExperimentList object containing assay
#'   data (expression, counts...), sample data and annotation data for the rows.
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
#' 
#' # 1: BASIC RNA-SEQ APP 
#' 
#' # Get an example RNA-seq dataset from the `airway` package
#'
#' data(airway, package = 'airway')
#'
#' Get some information about these data from the package description
#'
#' expinfo <- packageDescription('airway')
#'
#' # Convert to an ExploratorySummarizedExperiment (with extra slots)
#'
#' ese <- as(airway, 'ExploratorySummarizedExperiment')
#'
#' # Make the ExploratorySummarizedExperimentList that represents the study as a 
#' # whole.
#' 
#' eselist <- ExploratorySummarizedExperimentList(
#'   ese,
#'   title = expinfo$Title,
#'   author = expinfo$Author,
#'   description = expinfo$Description
#' )
#'
#' # Make the app
#'
#' app <- prepareApp('rnaseq', eselist)
#' 
#' # Run the app
#' 
#' shiny::shinyApp(ui = app$ui, server = app$server)
#' 
#' # 2: AUGMENT WITH ANNOTATION INFO FOR MORE INFORMATIVE APP
#' 
#' # Use Biomart to retrieve some annotation, and add it to the object
#' 
#' library(biomaRt)
#' attributes <- c(
#'   'ensembl_gene_id', # The sort of ID your results are keyed by
#'   'entrezgene', # Will be used mostly for gene set based stuff
#'   'external_gene_name' # Used to annotate gene names on the plot
#' )
#' 
#' mart <- useMart(biomart = 'ENSEMBL_MART_ENSEMBL', dataset = 'hsapiens_gene_ensembl', host='www.ensembl.org')
#' annotation <- getBM(attributes = attributes, mart = mart)
#' annotation <- annotation[order(annotation$entrezgene),]
#' 
#' mcols(ese) <- annotation[match(rownames(ese), annotation$ensembl_gene_id),]
#' ese@labelfield <- 'external_gene_name'
#'
#' # Re-do app creation etc, choose to use a different grouping variable by 
#' # default
#' 
#' eselist <- ExploratorySummarizedExperimentList(
#'   ese,
#'   title = expinfo$Title,
#'   author = expinfo$Author,
#'   description = expinfo$Description,
#'   default_groupvar <- 'dex' 
#' )
#' app <- prepareApp('rnaseq', eselist)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#' 
#' 3. MORE COMPLEX DATA FOR DIFFERENTIAL EXPRESSION ETC
#' 
#' # See vignette for more info. However, the included sample 
#' # ExploratorySummarizedExperimentList has the appopriate slots populated 
#' # to demonstrate contrasts, gene set annotations etc. The app produced in 
#' # this way will have more panels for differential analyses.
#' 
#' data('zhangneurons')
#' app <- prepareApp('rnaseq', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

prepareApp <- function(type, eselist, ui_only = FALSE, ...) {
    
    if (type %in% c("rnaseq", "chipseq", "illuminaarray")) {
        
        inputFunc <- get(paste0(type, "Input"))
        
        app <- list(ui = inputFunc(type, eselist), server = function(input, output, session) {
            callModule(get(type), type, eselist)
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
        
        ui <- fluidPage(includeCSS(cssfile), theme = shinythemes::shinytheme("cosmo"), shinyjs::useShinyjs(), navbarPage(id = "pages", title = moduletitle, windowTitle = moduletitle, 
            tabPanel(prettifyVariablename(module), sidebarLayout(sidebarPanel(inputFunc(module, eselist, ...), width = 3), mainPanel(outputFunc(module, ...), width = 9)))))
        
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
