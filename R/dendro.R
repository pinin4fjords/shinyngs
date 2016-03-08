#' The input function of the dendrogram module
#' 
#' This provides the form elements to control the pca display
#'
#' @param id Submodule namespace
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' dendroInput(ns('boxplot'), ses)

dendroInput <- function(id, ses) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("dendro"), ses)
    
    dendro_filters <- list(selectInput(ns("corMethod"), "Correlation method", c(Pearson = "pearson", Spearman = "spearman", Kendall = "kendall")), selectInput(ns("clusterMethod"), "Clustering method", c(`Ward minimum variance clustering` = "ward.D2", 
        `Single linkage` = "single", `Complete linkage` = "complete", `Average linkage` = "average", WPGMA = "mcquittye", UPGMC = "centroid")), uiOutput(ns("colorBy")))
    
    fieldSets(ns("fieldset"), list(clustering = dendro_filters, expression = expression_filters, export = plotdownloadInput(ns("dendro"))))
    
}

#' The output function of the boxplot module
#' 
#' This provides actual boxplot element for display by applications
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' dendroOutput('dendro')

dendroOutput <- function(id) {
    ns <- NS(id)
    plotOutput(ns("sampleDendroPlot"))
}

#' The server function of the dendrogram module
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
#' callModule(dendro, 'dendro', ses)

dendro <- function(input, output, session, ses) {
    
    # Get the expression matrix - no need for a gene selection
    
    selectmatrix_functions <- callModule(selectmatrix, "dendro", ses, select_genes = TRUE, var_n = 1000)
    
    selectMatrix <- selectmatrix_functions$selectMatrix
    matrixTitle <- selectmatrix_functions$title
    selectColData <- selectmatrix_functions$selectColData
    getExperiment <- selectmatrix_functions$getExperiment
    
    # Call to plotdownload module
    
    callModule(plotdownload, "dendro", makePlot = plotSampleDendroPlot, filename = "dendrogram.png", plotHeight = 600, plotWidth = 600)
    
    # Make the color selection UI element
    
    output$colorBy <- renderUI({
        ns <- session$ns
        se <- getExperiment()
        if ("group_vars" %in% names(metadata(se))) {
            selectInput(ns("colorBy"), "Color by", metadata(se)$group_vars, selected = metadata(se)$default_groupvar)
        }
    })
    
    # Reactive to get the color setting
    
    colorBy <- reactive({
        if ("colorBy" %in% names(input)) {
            return(input$colorBy)
        } else {
            return(NULL)
        }
    })
    
    # Reactive for making a plot for download
    
    plotSampleDendroPlot <- reactive({
        clustering_dendrogram(selectMatrix(), selectColData(), colorBy(), corMethod = input$corMethod, clusterMethod = input$clusterMethod, matrixTitle())
        
    })
    
    # Render the actual plot
    
    output$sampleDendroPlot <- renderPlot({
        withProgress(message = "Making sample dendrogram", value = 0, {
            
            clustering_dendrogram(selectMatrix(), selectColData(), colorBy(), corMethod = input$corMethod, clusterMethod = input$clusterMethod, matrixTitle())
            
        })
    }, height = 500)
}

#' Make a clustering dendrogram with coloring by experimental variable
#' 
#' A simple function using \code{ggdendro} to make a sample dendrogram
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#'
#' @return output A \code{ggplot} output
#'
#' @keywords keywords
#'
#' @import ggplot2
#' @import ggdendro
#' 
#' @export
#' 
#' @examples
#' ggplot_boxplot(selectMatrix(), selectColData(), colorBy())

clustering_dendrogram <- function(plotmatrix, experiment, colorby = NULL, corMethod = "pearson", clusterMethod = "ward.D", plot_title = "") {
    
    plotmatrix <- log2(plotmatrix + 1)
    
    dd <- as.dist(1 - cor(plotmatrix, method = corMethod))
    
    hc <- hclust(dd, method = clusterMethod)
    
    hcd <- as.dendrogram(hc)
    ddata_x <- ggdendro::dendro_data(hcd)
    
    p2 <- ggplot(ggdendro::segment(ddata_x)) + geom_segment(aes(x = x, y = y, xend = xend, yend = yend))
    
    labs <- ggdendro::label(ddata_x)
    
    ymax <- max(ddata_x$segments$yend)
    
    # Things are much simpler without coloring the samples
    
    if (is.null(colorby)) {
        
        p3 <- p2 + geom_text(data = labs, angle = 90, hjust = 1, size = rel(6), aes_string(label = "label", x = "x", y = -(ymax/40)), show_guide = F)
        
        p3 <- p3 + ggdendro::theme_dendro() + ylim(-(ymax/3), ymax)
        
        p3 <- p3 + geom_point(data = labs, aes_string(x = "x", y = 0), size = 4)
        
    } else {
        
        experiment[[colorby]][is.na(experiment[[colorby]])] <- "N/A"
        labs[[colorby]] <- as.character(experiment[[colorby]][match(labs$label, rownames(experiment))])
        shapes <- rep(15:20, 10)[1:length(unique(experiment[[colorby]]))]
        
        p3 <- p2 + geom_text(data = labs, angle = 90, hjust = 1, size = rel(6), aes_string(label = "label", x = "x", y = -(ymax/40), colour = colorby), show_guide = F)
        
        p3 <- p3 + ggdendro::theme_dendro() + ylim(-(ymax/3), ymax) + scale_color_discrete(name = colorby)
        
        p3 <- p3 + geom_point(data = labs, aes_string(x = "x", y = 0, colour = colorby, shape = colorby), size = 4) + scale_shape_manual(values = shapes) + theme(title = element_text(size = rel(1.8)), legend.text = element_text(size = rel(1.8))) + 
            ggtitle(plot_title)
        
    }
    
    print(p3 + theme(title = element_text(size = rel(1.8)), legend.text = element_text(size = rel(1.8))) + ggtitle(plot_title))
} 
