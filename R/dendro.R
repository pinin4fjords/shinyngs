#' The input function of the dendrogram module
#' 
#' This module will produce a sample clustering dendrogram based on 
#' user-selected parameters of row (e.g. gene) and column (sample) selection 
#' provided by the \code{selectmatrix} module, as well distance matrix 
#' generation and clustering method. 
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
#' dendroInput('myid', zhangneurons)

dendroInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("dendro"), eselist)
    
    dendro_filters <- list(selectInput(ns("corMethod"), "Correlation method", c(Pearson = "pearson", Spearman = "spearman", Kendall = "kendall")), selectInput(ns("clusterMethod"), 
        "Clustering method", c(`Ward minimum variance clustering` = "ward.D2", `Single linkage` = "single", `Complete linkage` = "complete", `Average linkage` = "average", 
            WPGMA = "mcquittye", UPGMC = "centroid")), groupbyInput(ns("dendro")), sliderInput(ns("labelspace"), label = "Label space (%)", min = 0.1, max = 0.5, 
        step = 0.05, value = 0.2))
    
    fieldSets(ns("fieldset"), list(clustering = dendro_filters, expression = expression_filters, export = plotdownloadInput(ns("dendro"))))
    
}

#' The output function of the dendro module
#' 
#' This module will produce a sample clustering dendrogram based on 
#' user-selected parameters of row (e.g. gene) and column (sample) selection 
#' provided by the \code{selectmatrix} module, as well distance matrix 
#' generation and clustering method. 
#' 
#' This provides actual dendrogram plot element for display by applications
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' dendroOutput('myid')

dendroOutput <- function(id) {
    ns <- NS(id)
    list(modalInput(ns("dendro"), "help", "help"), modalOutput(ns("dendro"), "Sample clustering dendrogram", includeMarkdown(system.file("inlinehelp", "dendro.md", 
        package = packageName()))), h3("Sample clustering dendrogram"), plotOutput(ns("sampleDendroPlot"), height = 600))
}

#' The server function of the dendrogram module
#' 
#' This module will produce a sample clustering dendrogram based on 
#' user-selected parameters of row (e.g. gene) and column (sample) selection 
#' provided by the \code{selectmatrix} module, as well distance matrix 
#' generation and clustering method. 
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
#' callModule(dendro, 'myid', eselist)

dendro <- function(input, output, session, eselist) {
    
    # Get the expression matrix - no need for a gene selection
    
    unpack.list(callModule(selectmatrix, "dendro", eselist, select_genes = TRUE, var_n = 1000, provide_all_genes = TRUE, default_gene_select = "variance"))
    unpack.list(callModule(groupby, "dendro", eselist = eselist, group_label = "Color by", selectColData = selectColData))
    
    # Call to plotdownload module
    
    callModule(plotdownload, "dendro", makePlot = plotSampleDendroPlot, filename = "dendrogram.png", plotHeight = 600, plotWidth = 800)
    
    # Reactive for making a plot for download
    
    plotSampleDendroPlot <- reactive({
        clusteringDendrogram(selectMatrix(), selectColData(), getGroupby(), cor_method = input$corMethod, cluster_method = input$clusterMethod, matrixTitle(), 
            palette = getPalette())
        
    })
    
    # Fetch the label spacing
    
    getLabelspace <- reactive({
        input$labelspace
    })
    
    # Render the actual plot
    
    output$sampleDendroPlot <- renderPlot({
        withProgress(message = "Making sample dendrogram", value = 0, {
            
            clusteringDendrogram(selectMatrix(), selectColData(), getGroupby(), cor_method = input$corMethod, cluster_method = input$clusterMethod, matrixTitle(), 
                labelspace = getLabelspace(), palette = getPalette())
            
        })
    }, height = 600)
}

#' Make a clustering dendrogram with coloring by experimental variable
#' 
#' A simple function using \code{ggdendro} to make a sample dendrogram
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param palette Palette of colors, one for each unique value derived from 
#' \code{colorby}.
#' @param cor_method Correlation method, passed to cor() (default: pearson).
#' @param cluster_method Clustering method, passed to hclust() (default: 
#' ward.D).
#' @param plot_title Plot title
#' @param labelspace Vertical fraction of plot to be used for labels (default: 0.2).
#'
#' @return output A \code{ggplot} output
#'
#' @keywords keywords
#'
#' @rawNamespace import(ggplot2, except = 'last_plot')
#' @import ggdendro
#' 
#' @export
#' 
#' @examples
#' # Make a dendrogram with the data in airway
#' 
#' data(airway, pakckage = 'airway')
#' clusteringDendrogram(assays(airway)[[1]], data.frame(colData(airway)), colorby = 'dex')
#' 
#' # Do the same, but only usig the 1000 most variant rows and see how the
#' # clustering improves. 
#' 
#' mymatrix <- assays(airway)[[1]]
#' mymatrix <- mymatrix[order(apply(mymatrix, 1, var), decreasing = TRUE)[1:1000],]
#' clusteringDendrogram(mymatrix, data.frame(colData(airway)), colorby = 'dex')

clusteringDendrogram <- function(plotmatrix, experiment, colorby = NULL, cor_method = "pearson", cluster_method = "ward.D", plot_title = "", labelspace = 0.2, 
    palette = NULL) {
    
    plotmatrix <- log2(plotmatrix + 1)
    
    hcd <- calculateDendrogram(plotmatrix, cor_method, cluster_method)
    
    ddata_x <- ggdendro::dendro_data(hcd)
    
    p2 <- ggplot(ggdendro::segment(ddata_x)) + geom_segment(aes(x = x, y = y, xend = xend, yend = yend))
    
    labs <- ggdendro::label(ddata_x)
    
    ymax <- max(ddata_x$segments$yend)
    
    # Things are much simpler without coloring the samples
    
    if (is.null(colorby)) {
        
        p3 <- p2 + geom_text(data = labs, angle = 90, hjust = 1, size = rel(6), aes_string(label = "label", x = "x", y = -(ymax/40)), show.legend = F)
        
        p3 <- p3 + ggdendro::theme_dendro() + ylim(-(ymax/3), ymax)
        
        p3 <- p3 + geom_point(data = labs, aes_string(x = "x", y = 0), size = 4)
        
    } else {
        
        labs[[colorby]] <- as.character(experiment[[colorby]][match(labs$label, rownames(experiment))])
        labs[[colorby]] <- na.replace(labs[[colorby]], replacement = "N/A")
        
        labs[[colorby]] <- factor(labs[[colorby]], levels = unique(na.replace(experiment[[colorby]], "N/A")))
        shapes <- rep(15:20, 10)[1:length(unique(experiment[[colorby]]))]
        
        p3 <- p2 + geom_text(data = labs, angle = 90, hjust = 1, size = rel(5), aes_string(label = "label", x = "x", y = -(ymax/40), colour = colorby), show.legend = F)
        
        total_axis_size = ymax * (1/(1 - labelspace))
        
        p3 <- p3 + ggdendro::theme_dendro() + ylim(-(total_axis_size * labelspace), ymax) + scale_color_manual(name = prettifyVariablename(colorby), values = palette)
        
        p3 <- p3 + geom_point(data = labs, aes_string(x = "x", y = 0, colour = colorby, shape = colorby), size = 4) + scale_shape_manual(values = shapes, name = prettifyVariablename(colorby)) + 
            theme(title = element_text(size = rel(1.8)), legend.text = element_text(size = rel(1.8))) + ggtitle(plot_title)
        
    }
    
    if (!is.null(colorby)) {
        p3 <- p3 + guides(color = guide_legend(nrow = ceiling(length(unique(experiment[[colorby]]))/2)))
    }
    print(p3 + theme(title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)), legend.position = "bottom") + ggtitle(plot_title))
}

#' Calculate a distance matrix based on correlation
#'
#' @param plotmatrix Expression/ other data matrix
#' @param cor_method 'spearman' or 'perason'
#'
#' @return output Object of class 'dist'
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' data(airway, package = 'airway')
#' mymatrix <- assays(airway)[[1]]
#' calculateDist(mymatrix)

calculateDist <- function(plotmatrix, cor_method = "spearman") {
    as.dist(1 - cor(plotmatrix, method = cor_method))
}

#' Calculate a clustering dendgrogram based on correlation
#'
#' @param plotmatrix Expression/ other data matrix
#' @param cor_method 'spearman' or 'perason'
#' @param cluster_method Clustering method to pass to hclust (Default: 'ward.D2')
#'
#' @return output Object of class 'dist'
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' data(airway, package = 'airway')
#' mymatrix <- assays(airway)[[1]]
#' calculateDendrogram(mymatrix)

calculateDendrogram <- function(plotmatrix, cor_method = "spearman", cluster_method = "ward.D2") {
    
    dd <- calculateDist(plotmatrix, cor_method = cor_method)
    
    hc <- hclust(dd, method = cluster_method)
    
    as.dendrogram(hc)
} 
