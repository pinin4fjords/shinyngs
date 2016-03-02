#' The input function of the pca module
#' 
#' This provides the form elements to control the pca display
#'
#' @param id Submodule namespace
#' @param se StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#' @param principal_components A named vector of princpal components to provide. Default = c('PC1' = 1 ... 'PC10' = 10).
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' pcaInput('pca', se, group_vars, default_groupvar, tructure(1:10, names=paste0('PC', 1:10)))

pcaInput <- function(id, se, principal_components = structure(1:10, names = paste0("PC", 1:10))) {
    
    ns <- NS(id)
    
    inputs <- list(h3("Principal component analysis"), selectmatrixInput(ns("pca"), se), h4("Set plotting parameters"), selectInput(ns("xAxisComponent"), 
        "x axis component", principal_components, selected = 1), selectInput(ns("yAxisComponent"), "y axis component", principal_components, 
        selected = 2), selectInput(ns("zAxisComponent"), "z axis component", principal_components, selected = 3))
    
    if ("group_vars" %in% names(metadata(se))) {
        inputs[[length(inputs) + 1]] <- selectInput(ns("samplePCAColorBy"), "Color by", metadata(se)$group_vars, selected = metadata(se)$default_groupvar)
    }
    
    tagList(inputs)
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
#' @param se StructuredExperiment object with assay and experimental data, with
#' additional information in the metadata() slot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(pca, 'pca', se, params$transcriptfield, params$entrezgenefield, params$genefield, geneset_files = params$geneset_files)

pca <- function(input, output, session, se) {
    
    selectmatrix_functions <- callModule(selectmatrix, "pca", se, var_n = 1000, var_max = nrow(se))
    selectMatrix <- selectmatrix_functions$selectMatrix
    matrixTitle <- selectmatrix_functions$title
    selectColData <- selectmatrix_functions$selectColData
    
    colorby <- reactive({
        if ("samplePCAColorBy" %in% names(input)) {
            return(input$samplePCAColorBy)
        } else {
            return(NULL)
        }
    })
    
    output$samplePCAPlot <- renderPlotly({
        withProgress(message = "Making interactive 3D PCA plot", value = 0, {
            
            if (nrow(selectMatrix()) > 0) {
                plotlyPCA(selectMatrix(), as.numeric(input$xAxisComponent), as.numeric(input$yAxisComponent), as.numeric(input$zAxisComponent), 
                  selectColData(), colorby(), matrixTitle())
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

plotlyPCA <- function(pcavals, pcX, pcY, pcZ, pcameta, colorby = NULL, title = "foo") {
    
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
    
    plotdata$name <- rownames(plotdata)
    
    if (!is.null(colorby)) {
        plotdata$color <- factor(pcameta[match(rownames(plotdata), rownames(pcameta)), colorby], levels = unique(pcameta[, colorby]))
        p <- plot_ly(plotdata, x = plotdata[, pcX], y = plotdata[, pcY], z = plotdata[, pcZ], type = "scatter3d", mode = "markers", 
            color = color, text = plotdata$name, hoverinfo = "text")
    } else {
        p <- plot_ly(plotdata, x = plotdata[, pcX], y = plotdata[, pcY], z = plotdata[, pcZ], type = "scatter3d", mode = "markers", 
            text = plotdata$name, hoverinfo = "text")
    }
    
    p <- layout(p, xaxis = list(title = colnames(plotdata)[pcX]), yaxis = list(title = colnames(plotdata)[pcY]), zaxis = list(title = colnames(plotdata)[pcZ]), 
        scene = list(xaxis = list(title = colnames(plotdata)[pcX]), yaxis = list(title = colnames(plotdata)[pcY]), zaxis = list(title = colnames(plotdata)[pcZ])), 
        margin = list(l = 0, r = 0, t = 50, b = 0), legend = list(y = 0.8), title = title)
    
    p
    
} 
