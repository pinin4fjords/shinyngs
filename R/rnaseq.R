#' The input function of the rnaseq module
#' 
#' This provides the form elements to control the RNA-seq display
#' 
#' The rnaseq module is a combination of output from many modules (pca, boxplot 
#' etc) to form a comprehensive analysis application.
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
#' rnaseqInput('rnaseq', eselist)

rnaseqInput <- function(id, eselist) {
    ns <- NS(id)
    
    navbar_menus <- list(id = ns("rnaseq"), title = paste0("RNA-seq explorer: ", eselist@title), windowTitle = eselist@title, tabPanel("Home", 
        sidebarLayout(sidebarPanel(column(12, offset = 0, p(HTML("This is an interface designed to facilitate downstream RNA-seq (and similar) analysis. It is generated using the Shinyngs package, which makes extensive use of <a href='http://shiny.rstudio.com/'>Shiny</a> and related packages.")), 
            p(HTML(paste0(icon("github"), "&nbsp;Please report any bugs you see to <a href='https://github.com/pinin4fjords/shinyngs'>Shinyngs's Github page</a>"))), 
            p(HTML(paste0(icon("chrome"), "&nbsp;This app is best viewed with the Chrome browser.")))), width = 3), mainPanel(fluidRow(column(12, 
            offset = 0, h2(eselist@title), h3(eselist@author), HTML(eselist@description))), width = 9)), icon = icon("home")), navbarMenu("Sample data", 
        tabPanel("Experiment", sidebarLayout(sidebarPanel(experimenttableInput(ns("experimenttable"), eselist), width = 3), mainPanel(experimenttableOutput(ns("experimenttable")), 
            width = 9)), icon = icon("table")), tabPanel("Annotation", sidebarLayout(sidebarPanel(rowmetatableInput(ns("rowmetatable"), 
            eselist), width = 2), mainPanel(rowmetatableOutput(ns("rowmetatable")), width = 10)), icon = icon("table")), icon = icon("flask")))
    
    # Add in the QC/ exploratory menu
    
    exploratory_menu <- list("QC/ exploratory", tabPanel("Quartile plots", sidebarLayout(sidebarPanel(boxplotInput(ns("boxplot"), 
        eselist), width = 3), mainPanel(boxplotOutput(ns("boxplot")), width = 9)), icon = icon("bar-chart-o")), tabPanel("PCA", 
        sidebarLayout(sidebarPanel(pcaInput(ns("pca"), eselist), width = 3), mainPanel(pcaOutput(ns("pca")), width = 9)), icon = icon("cube")), 
        tabPanel("PCA vs Experiment", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-pca"), eselist, type = "pca"), width = 3), 
            mainPanel(heatmapOutput(ns("heatmap-pca"), type = "pca"), width = 9)), icon = icon("cubes")), tabPanel("Clustering dendrogram", 
            sidebarLayout(sidebarPanel(dendroInput(ns("dendro"), eselist), width = 3), mainPanel(dendroOutput(ns("dendro")), width = 9)), 
            icon = icon("sitemap")), tabPanel("Clustering Heatmap", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-clustering"), 
            eselist, type = "samples"), width = 3), mainPanel(heatmapOutput(ns("heatmap-clustering"), type = "samples"), width = 9)), 
            icon = icon("th")))
    
    # Add read reports if provided
    
    if (length(eselist@read_reports) > 0) {
        exploratory_menu <- pushToList(exploratory_menu, tabPanel("Read reports", sidebarLayout(sidebarPanel(readreportsInput(ns("readrep"), 
            eselist), width = 3), mainPanel(readreportsOutput(ns("readrep")), width = 9)), icon = icon("bar-chart-o")))
    }
    exploratory_menu$icon <- icon("binoculars")
    
    navbar_menus <- pushToList(navbar_menus, do.call("navbarMenu", exploratory_menu))
    
    # Add the assay data menu
    
    assaydata_menu <- list("Assay data", tabPanel("Tables", sidebarLayout(sidebarPanel(assaydatatableInput(ns("expression"), eselist), 
        width = 3), mainPanel(assaydatatableOutput(ns("expression")), width = 9)), icon = icon("table")), tabPanel("Heatmaps", sidebarLayout(sidebarPanel(heatmapInput(ns("heatmap-expression"), 
        eselist, type = "expression"), width = 3), mainPanel(heatmapOutput(ns("heatmap-expression"), type = "expression"), width = 9)), 
        icon = icon("th")))
    
    assaydata_menu$icon <- icon("table")
    
    navbar_menus <- pushToList(navbar_menus, do.call("navbarMenu", assaydata_menu))
    
    # If there are contrasts present, add the differential tab
    
    if (length(eselist@contrasts) > 0) {
        
        differential_menu <- list("Differential", tabPanel("Tables", sidebarLayout(sidebarPanel(differentialtableInput(ns("differential"), 
            eselist), width = 3), mainPanel(differentialtableOutput(ns("differential")), width = 9)), icon = icon("table")), tabPanel("Fold change plots", 
            sidebarLayout(sidebarPanel(foldchangeplotInput(ns("foldchange"), eselist), width = 3), mainPanel(foldchangeplotOutput(ns("foldchange")), 
                width = 9)), icon = icon("line-chart")), tabPanel("MA plots", sidebarLayout(sidebarPanel(maplotInput(ns("ma"), eselist), 
            width = 3), mainPanel(maplotOutput(ns("ma")), width = 9)), icon = icon("line-chart")))
        
        # If any of the experiments in the list have assays with associated tests, add a volcano plot
        
        if (any(unlist(lapply(eselist, function(ese) {
            length(ese@tests) > 0
        })))) {
            differential_menu <- pushToList(differential_menu, tabPanel("Volcano plots", sidebarLayout(sidebarPanel(volcanoplotInput(ns("volcano"), 
                eselist), width = 3), mainPanel(volcanoplotOutput(ns("volcano")), width = 9)), icon = icon("line-chart")))
        }
        
        # If any of the experiments have gene set analyses, add this table to the menu
        
        if (any(unlist(lapply(eselist, function(ese) {
            length(ese@gene_set_analyses) > 0
        })))) {
            differential_menu <- pushToList(differential_menu, tabPanel("Gene set analyses", sidebarLayout(sidebarPanel(genesetanalysistableInput(ns("genesetanalysis"), 
                eselist), width = 3), mainPanel(genesetanalysistableOutput(ns("genesetanalysis")), width = 9)), icon = icon("tasks")))
            
            differential_menu <- pushToList(differential_menu, tabPanel("Gene set barcode plots", value = "genesetbarcode", sidebarLayout(sidebarPanel(genesetbarcodeplotInput(ns("rnaseq"), 
                eselist), width = 3), mainPanel(genesetbarcodeplotOutput(ns("rnaseq")), width = 9)), icon = icon("barcode")))
            
        }
        
        # If any of the experiments have differential exon usage results
        
        if (any(unlist(lapply(eselist, function(ese) {
            length(ese@dexseq_results) > 0
        })))) {
            differential_menu <- pushToList(differential_menu, tabPanel("Differential exon usage table", sidebarLayout(sidebarPanel(dexseqtableInput(ns("deutable"), 
                eselist), width = 3), mainPanel(dexseqtableOutput(ns("deutable")), width = 9))))
            differential_menu <- pushToList(differential_menu, tabPanel("Differential exon usage plot", value = "deugene", sidebarLayout(sidebarPanel(dexseqplotInput(ns("deuplot"), 
                eselist), width = 3), mainPanel(dexseqplotOutput(ns("deuplot")), width = 9))))
        }
        
        differential_menu$icon <- icon("line-chart")
        
        navbar_menus <- pushToList(navbar_menus, do.call("navbarMenu", differential_menu))
        
    }
    
    # Add the gene info plots
    
    navbar_menus <- pushToList(navbar_menus, tabPanel("Gene info", value = "geneinfo", sidebarLayout(sidebarPanel(geneInput(ns("gene"), 
        eselist), width = 3), mainPanel(geneOutput(ns("gene"), eselist), width = 9)), icon = icon("bar-chart-o")))
    
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
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'   
#' @keywords shiny
#'   
#' @examples
#' callModule(rnaseq, 'rnaseq', eselist)

rnaseq <- function(input, output, session, eselist) {
    
    # Add internal links to the tables with gene labels
    
    for (esen in names(eselist)) {
        ese <- eselist[[esen]]
        
        if (length(ese@labelfield) > 0) {
            eselist@url_roots[[ese@labelfield]] <- "?gene="
            eselist@url_roots$significant_genes <- "?gene="
            eselist@url_roots$gene_set_id <- "?geneset="
        }
    }
    
    # Now a lot of boring calls to all the modules to activate the UI parts
    
    callModule(experimenttable, "experimenttable", eselist)
    callModule(rowmetatable, "rowmetatable", eselist)
    callModule(heatmap, "heatmap-clustering", eselist, type = "samples")
    callModule(heatmap, "heatmap-expression", eselist, type = "expression")
    callModule(heatmap, "heatmap-pca", eselist, type = "pca")
    callModule(pca, "pca", eselist)
    callModule(boxplot, "boxplot", eselist)
    callModule(dendro, "dendro", eselist)
    callModule(assaydatatable, "expression", eselist)
    
    # Calls for the various optional tables
    
    if (length(eselist@read_reports) > 0) {
        callModule(readreports, "readrep", eselist)
    }
    
    if (length(eselist@contrasts) > 0) {
        callModule(differentialtable, "differential", eselist)
        callModule(volcanoplot, "volcano", eselist)
        callModule(foldchangeplot, "foldchange", eselist)
        callModule(maplot, "ma", eselist)
        callModule(genesetanalysistable, "genesetanalysis", eselist)
        updateBarcodeGeneset <- callModule(genesetbarcodeplot, "rnaseq", eselist)
    }
    
    if (any(unlist(lapply(eselist, function(ese) {
        length(ese@dexseq_results) > 0
    })))) {
        callModule(dexseqtable, "deutable", eselist)
        updateDEUGeneLabel <- callModule(dexseqplot, "deuplot", eselist)
    }
    
    updateGeneLabel <- callModule(gene, "gene", eselist)
    
    # Catch the specified gene from the URL, switch to the gene info tab, and and use the reactive supplied by the gene module to
    # update its gene label field accordingly
    
    observe({
        query <- parseQueryString(session$clientData$url_search)
        
        if (length(intersect(c("gene", "geneset", "deu_gene"), names(query))) == 0) {
            return()
        }
        
        url_observe <- observe({
            if ("deu_gene" %in% names(query)) {
                updateNavbarPage(session, "rnaseq", "deugene")
                updateDEUGeneLabel()
            } else if ("gene" %in% names(query)) {
                updateNavbarPage(session, "rnaseq", "geneinfo")
                updateGeneLabel()
            } else if ("geneset" %in% names(query)) {
                updateNavbarPage(session, "rnaseq", "genesetbarcode")
                updateBarcodeGeneset()
            }
            url_observe$suspend()
        })
    })
    
}
