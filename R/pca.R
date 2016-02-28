#' The input function of the pca module
#' 
#' This provides the form elements to control the pca display
#'
#' @param id Submodule namespace
#' @param se StructuredExperiment object with assay and experimental data
#' @param group_vars The variables from the structured experiment that should
#' be used to control sample grouping in the plot
#' @param The default grouping variable to use
#' @param principal_components A named vector of princpal components to provide. Default = c('PC1' = 1 ... 'PC10' = 10).
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' pcaInput('pca', se, group_vars, default_groupvar, tructure(1:10, names=paste0('PC', 1:10)))

pcaInput <- function(id, se, group_vars, default_groupvar, principal_components = structure(1:10, names = paste0("PC", 1:10))) {
    
    ns <- NS(id)
    
    tagList(h3("Principal component analysis"), selectmatrixInput(ns("pca"), se, group_vars, default_groupvar), h4("Set plotting parameters"), 
        selectInput(ns("xAxisComponent"), "x axis component", principal_components, selected = 1), selectInput(ns("yAxisComponent"), "y axis component", 
            principal_components, selected = 2), selectInput(ns("zAxisComponent"), "z axis component", principal_components, selected = 3), selectInput(ns("samplePCAColorBy"), 
            "Color by", group_vars, selected = default_groupvar))
}

#' The output function of the pca module
#' 
#' This provides actual pca element for display by applications
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' pcaOutput('pca')

pcaOutput <- function(id) {
    ns <- NS(id)
    plotlyOutput(ns("samplePCAPlot"), height = 600)
}

#' The server function of the pca module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' Matrix and UI selection elements provided by the selectmatrix module
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param se StructuredExperiment object with assay and experimental data
#' @param transcriptfield The main identifier for the rows in the assay data.
#' This could be transcript ID, but also probe etc.
#' @param entrezgenefield The column of annotation containing Entrez gene IDs
#' @param genefield The gene ID type in annotation by which results are keyed
#' @param geneset_files (optional) A named list of .gmt gene set files as might be 
#' derived from MSigDB
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(pca, 'pca', se, params$transcriptfield, params$entrezgenefield, params$genefield, geneset_files = params$geneset_files)

pca <- function(input, output, session, se, transcriptfield, entrezgenefield, genefield, geneset_files = NULL) {
    
    selectmatrix_functions <- callModule(selectmatrix, "pca", se, transcriptfield, entrezgenefield, genefield, geneset_files, var_n = 1000)
    selectMatrix <- selectmatrix_functions$selectMatrix
    matrixTitle <- selectmatrix_functions$title
    selectColData <- selectmatrix_functions$selectColData
    
    output$samplePCAPlot <- renderPlotly({
        withProgress(message = "Making interactive 3D PCA plot", value = 0, {
            
            if (nrow(selectMatrix()) > 0) {
                plotlyPCA(selectMatrix(), as.numeric(input$xAxisComponent), as.numeric(input$yAxisComponent), as.numeric(input$zAxisComponent), 
                  selectColData(), input$samplePCAColorBy, matrixTitle())
            }
        })
    })
}

#' Make the PCA plot using plotly
#' 
#' Calculates a principal components analysis and plots it in 3D using the 
#' plotly library. Takes a few seconds for plotly to do its thing.
#'
#' @param pcavals Matrix of values
#' @param pcaX Integer value between 1 and 10 specifying which component to plot on the X axis
#' @param pcaX Integer value between 1 and 10 specifying which component to plot on the Y axis
#' @param pcaZ Integer value between 1 and 10 specifying which component to plot on the Z axis
#' @param pcameta Data frame containing metadata to use for coloring etc
#' @param colorby Column name in \code{pcameta} specifying how points should be colored
#' @param title String to use as title in plot 
#'
#' @return output Plotly plot object 
#' 
#' @examples
#' plotlyPCA(
#'   mymatrix, 
#'   1,
#'   2,
#'   3,
#'   pcameta=myexperiment, 
#'   colorby=myvariable,
#'   title='My title'
#' ) 

plotlyPCA <- function(pcavals, pcX, pcY, pcZ, pcameta, colorby, title = "foo") {
    
    if (min(pcavals) == 0) {
        pcavals <- pcavals + 1
    }
    
    vars <- apply(pcavals, 1, var)
    pcavals <- pcavals[vars > 0, ]
    pcavals <- log2(pcavals)
    
    pca <- prcomp(t(pcavals), scale = T)
    
    fraction_explained <- round((pca$sdev)^2/sum(pca$sdev^2), 3) * 100
    
    plotdata <- data.frame(pca$x)
    colnames(plotdata) <- paste0(colnames(plotdata), ": ", fraction_explained, "%")
    
    plotdata$color <- factor(pcameta[match(rownames(plotdata), rownames(pcameta)), colorby], levels = unique(pcameta[, colorby]))
    plotdata$name <- rownames(plotdata)
    
    p <- plot_ly(plotdata, x = plotdata[, pcX], y = plotdata[, pcY], z = plotdata[, pcZ], type = "scatter3d", mode = "markers", color = color, 
        text = plotdata$name, hoverinfo = "text")
    
    p <- layout(p, xaxis = list(title = colnames(plotdata)[pcX]), yaxis = list(title = colnames(plotdata)[pcY]), zaxis = list(title = colnames(plotdata)[pcZ]), 
        scene = list(xaxis = list(title = colnames(plotdata)[pcX]), yaxis = list(title = colnames(plotdata)[pcY]), zaxis = list(title = colnames(plotdata)[pcZ])), 
        margin = list(l = 0, r = 0, t = 50, b = 0), legend = list(y = 0.8), title = title)
    
    p
    
} 
