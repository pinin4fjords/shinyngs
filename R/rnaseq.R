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
    
    navbar_menus <- list(id = ns("rnaseq"), title = paste0("RNA-seq explorer: ", title), windowTitle = title, tabPanel("Home", sidebarLayout(sidebarPanel(width = 3), 
        mainPanel(width = 9))), navbarMenu("Sample data", tabPanel("Experiment", sidebarLayout(sidebarPanel(experimenttableInput(ns("experimenttable"), ses), width = 3), 
        mainPanel(experimenttableOutput(ns("experimenttable")), width = 9)))), navbarMenu("QC/ exploratory", tabPanel("Boxplots", sidebarLayout(sidebarPanel(boxplotInput(ns("boxplot"), 
        ses), width = 3), mainPanel(boxplotOutput(ns("boxplot")), width = 9))), tabPanel("PCA", sidebarLayout(sidebarPanel(pcaInput(ns("pca"), ses), width = 3), mainPanel(pcaOutput(ns("pca")), 
        width = 9))), tabPanel("PCA vs Experiment", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-pca"), ses, type = "pca"), width = 3), mainPanel(heatmapOutput(ns("heatmap-pca")), 
        width = 9))), tabPanel("Clustering dendrogram", sidebarLayout(sidebarPanel(dendroInput(ns("dendro"), ses), width = 3), mainPanel(dendroOutput(ns("dendro")), width = 9))), 
        tabPanel("Clustering Heatmap", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-clustering"), ses, type = "samples"), width = 3), mainPanel(heatmapOutput(ns("heatmap-clustering")), 
            width = 9)))), navbarMenu("Assay data", tabPanel("Tables", sidebarLayout(sidebarPanel(assaydatatableInput(ns("expression"), ses), width = 3), mainPanel(assaydatatableOutput(ns("expression")), 
        width = 9))), tabPanel("Heatmaps", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-expression"), ses, type = "expression"), width = 3), mainPanel(heatmapOutput(ns("heatmap-expression")), 
        width = 9)))))
    
    # If there are contrasts present, add the differential tab
    
    if (any(unlist(lapply(ses, function(se) "contrasts" %in% names(metadata(se)))))) {
        navbar_menus <- pushToList(navbar_menus, navbarMenu("Differential", tabPanel("Tables", sidebarLayout(sidebarPanel(differentialtableInput(ns("differential"), ses), 
            width = 3), mainPanel(differentialtableOutput(ns("differential")), width = 9)))))
    }
    
    # Add the gene info plots
    
    navbar_menus <- pushToList(navbar_menus, tabPanel("Gene info", value = "geneinfo", sidebarLayout(sidebarPanel(geneInput(ns("gene"), ses), width = 3), mainPanel(geneOutput(ns("gene")), 
        width = 9))))
    
    # Add the final wrappers
    
    cssfile <- system.file("www", paste0(packageName(), ".css"), package = packageName())
    fluidPage(includeCSS(cssfile), theme = shinythemes::shinytheme("cosmo"), shinyjs::useShinyjs(), do.call(navbarPage, navbar_menus))
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
    
    # Add internal links to the tables with gene labels
    
    for (sen in names(ses)) {
        se <- ses[[sen]]
        
        if ("labelfield" %in% names(metadata(ses[[sen]]))) {
            labelfield <- metadata(ses[[sen]])$labelfield
            metadata(ses[[sen]])$url_roots[[labelfield]] <- "?gene="
        }
    }
    
    callModule(experimenttable, "experimenttable", ses)
    callModule(heatmap, "heatmap-clustering", ses, type = "samples")
    callModule(heatmap, "heatmap-expression", ses, type = "expression")
    callModule(heatmap, "heatmap-pca", ses, type = "pca")
    callModule(pca, "pca", ses)
    callModule(boxplot, "boxplot", ses)
    callModule(dendro, "dendro", ses)
    callModule(assaydatatable, "expression", ses)
    callModule(differentialtable, "differential", ses)
    updateGeneLabel <- callModule(gene, "gene", ses)
    
    # Catch the specified gene from the URL, switch to the gene info tab, and and use the reactive supplied by the gene module to update its gene label field accordingly
    
    observe({
        query <- parseQueryString(session$clientData$url_search)
        
        if (length(intersect(c("gene"), names(query))) == 0) {
            return()
        }
        
        url_observe <- observe({
            if ("gene" %in% names(query)) {
                updateNavbarPage(session, "rnaseq", "geneinfo")
                updateGeneLabel()
            }
            url_observe$suspend()
        })
    })
    
} 
