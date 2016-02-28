#' The input function of the heatmap module
#' 
#' This provides the form elements to control the heatmap display
#'
#' @param id Submodule namespace
#' @param se StructuredExperiment object with assay and experimental data
#' @param group_vars The variables from the structured experiment that should
#' be used to control sample grouping in the plot
#' @param The default grouping variable to use
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' heatmapInput('heatmap', se, group_vars, default_groupvar

heatmapInput <- function(id, se, group_vars, default_groupvar) {
    
    ns <- NS(id)
    
    tagList(selectmatrixInput(ns("heatmap"), se, group_vars, default_groupvar), h4("Set plotting parameters"), div(h5("Clustering"), checkboxInput(ns("cluster_rows"), "Cluster rows?", 
        TRUE), checkboxInput(ns("cluster_cols"), "Cluster columns?", FALSE)), radioButtons(ns("scale"), "Scale by:", c(Row = "row", Column = "column", None = "none")), 
        checkboxGroupInput(ns("groupVars"), "Annotate with variables:", group_vars, selected = group_vars, inline = TRUE), downloadButton(ns("downloadHeatMap"), "Download Plot"))
    
}

#' The output function of the heatmap module
#' 
#' This provides actual heatmap element for display by applications
#'
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' heatmapOutput('heatmap')

heatmapOutput <- function(id) {
    ns <- NS(id)
    plotOutput(ns("heatMap"))
}

#' The server function of the heatmap module
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' This function assumes that the gene sets have one gene ID (e.g. Entrez)
#' which need to be converted to another (e.g. Symbol, Ensembl gene ID).
#' This would be common when dealign with MSigDB gene sets, for example.
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param se StructuredExperiment object with assay and experimental data
#' @param transcriptfield The main identifier for the rows in the assay data.
#' This could be transcript ID, but also probe etc.
#' @param entrezgenefield The column of annotation containing Entrez gene IDs
#' @param genefield The gene ID type in annotation by which results are keyed
#' @param geneset_files A named list of .gmt gene set files as might be 
#' derived from MSigDB
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(heatmap, 'heatmap', se, params$transcriptfield, params$entrezgenefield, params$genefield, geneset_files=params$geneset_files)

heatmap <- function(input, output, session, se, transcriptfield, entrezgenefield, genefield, geneset_files = NULL) {
    
    # selectSamples <- callModule(sampleselect, 'heatmap', se)
    
    # Call the geneselect module's server function. This will return accessors for the gene sets (if appropriate).  Takes selectColumns() reactive as an argument. This is
    # because we might need to calculate the variance of rows after column selection.
    
    # geneselect_functions <- callModule(geneselect, 'heatmap', se, transcriptfield, entrezgenefield, genefield, geneset_files, selectSamples=selectSamples) selectRows <-
    # geneselect_functions$selectRows
    
    selectmatrix_functions <- callModule(selectmatrix, "heatmap", se, transcriptfield, entrezgenefield, genefield, geneset_files)
    selectMatrix <- selectmatrix_functions$selectMatrix
    matrixTitle <- selectmatrix_functions$title
    
    # Select out the expression values we need for a heatmap
    
    getHeatmapExpression <- reactive({
        
        withProgress(message = "Getting values for heatmap", value = 0, {
            
            # heatmap_expression <- se[selectRows(),selectSamples()]
            
            heatmap_expression <- selectMatrix()
            
            # Name the rows
            
            if (nrow(heatmap_expression) > 0) {
                
                annotation <- data.frame(mcols(se))
                display_genes <- annotation[match(rownames(heatmap_expression), annotation[[transcriptfield]]), genefield]
                hasgenes <- !is.na(display_genes)
                
                rownames(heatmap_expression)[hasgenes] <- paste(display_genes[hasgenes], rownames(heatmap_expression)[hasgenes], sep = " / ")
                
                return(heatmap_expression)
            } else {
                return(NULL)
            }
        })
    })
    
    # Calculate the plot height based on the inputs
    
    heatmapPlotheight <- reactive({
        
        # validate(need(!is.null(input$geneSelect), 'Waiting for form to provide geneSelect'))
        
        nrows <- 0
        titleheight = 25
        labelsheight = 80
        
        return((titleheight * nlines(matrixTitle())) + (length(input$groupVars) * 14) + (nrow(selectMatrix()) * 12) + labelsheight)
    })
    
    plotwidth <- reactive({
        return(400 + (18 * ncol(se)))
    })
    
    # Supply a rendered heatmap for display
    
    output$heatMap <- renderPlot({
        expression <- getHeatmapExpression()
        
        makeHeatmap(input, expression, main = matrixTitle())
    }, height = heatmapPlotheight)
    
    # Provide the heatmap for download using the following two functions
    
    plotInput <- reactive({
        makeHeatmap(input, getHeatmapExpression(), main = matrixTitle())
    })
    
    output$downloadHeatMap <- downloadHandler(filename = "heatmap.png", content = function(file) {
        png(file, height = heatmapPlotheight(), width = plotwidth(), units = "px")
        print(plotInput())
        dev.off()
    })
    
}

#' Make the heatmap
#' 
#' Just an adapter to annotatedHeatmap(), which is designed to be generically
#' useful
#'
#' @param input Input object
#' @param se StructuredExperiment object with assay and experimental data
#' @param assay The name or index of the assay in \code{se} that should be used
#' @param ... Arguments that should be passed through \code{annotatedHeatmap()}
#' to \code{pheatmap()}
#'
#' @return output Heatmap plot 
#' 
#' @examples
#' makeHeatmap(input, se, main = 'title')

makeHeatmap <- function(input, se, assay = 1, ...) {
    
    if (!is.null(se)) {
        
        # We can't do clustering with anything with the same value in all columns. So take these out.
        
        if (input$cluster_rows) {
            se <- se[apply(assays(se)[[assay]], 1, function(x) length(unique(x)) > 1), ]
        }
        
        # If the above 'if' reduced the frame to 1 row we can't do clustering anyway
        
        if (nrow(se) > 1) {
            annotatedHeatmap(log2(assays(se)[[assay]] + 1), data.frame(colData(se)), group_vars = input$groupVars, cluster_rows = input$cluster_rows, cluster_cols = input$cluster_cols, 
                scale = input$scale, ...)
        }
    }
}

#' Make a heatmap with annotations by experimental variable
#' 
#' This is a generic function which may be useful outside of this package. It
#' produces a heatmap based on an expression matrix and accompanying 
#' experiment data in the form of a frame, using pheatmap().
#'
#' @param plotmatrix Expression/ other data matrix
#' @param sample_annotation Annotation for the columns of plotmatrix
#' @param group_vars Grouping variables to be used from 
#' sample_annotation
#' @param ... Other arguments to pass to \code{pheatmap()}
#'
#' @return output A plot as produced by pheatmap() 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

annotatedHeatmap <- function(plotmatrix, sample_annotation, group_vars = NULL, ...) {
    
    if (!is.null(plotmatrix)) {
        
        sample_annotation <- data.frame(apply(sample_annotation[colnames(plotmatrix), rev(group_vars), drop = F], 2, as.factor))
        
        colors <- makeAnnotationColors(sample_annotation)
        
        pheatmap::pheatmap(plotmatrix, show_rownames = T, fontsize = 12, fontsize_row = 10, cellheight = 12, annotation = sample_annotation, annotation_colors = colors, 
            border_color = NA, legend = FALSE, ...)
    }
}

#' Make color sets to use in heatmap annotation
#' 
#' Given a data frame of experimental variables, generate sets of colors, one 
#' for each column, than can be used as the 'annotation_colors' argment to 
#' \code{pheatmap()}. Uses \code{RColorBrewer}.
#'
#' @param sample_annotation A data frame
#'
#' @return output A list object with colors 
#'
#' @export
#' 
#' @examples
#' colors <- makeAnnotationColors(sample_annotation)

makeAnnotationColors <- function(sample_annotation) {
    
    palettes <- rep(c("Set1", "Set2", "Set3", "Dark2", "Accent"), 100)
    
    colors = list()
    colors_so_far = 0
    for (i in 1:ncol(sample_annotation)) {
        if (!is.factor(sample_annotation[, i])) {
            sample_annotation[, i] <- as.factor(sample_annotation[, i])
        }
        
        categories <- unique(as.numeric(sample_annotation[, i]))
        
        # If the palette is longer than the number of colors required, then we're good, otherwise we have to do some interpolation
        
        if (RColorBrewer::brewer.pal.info[palettes[i], "maxcolors"] >= length(categories) && length(categories) > 2) {
            colcolors <- RColorBrewer::brewer.pal(length(categories), palettes[i])
        } else {
            colcolors <- sample(colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palettes[i], "maxcolors"], palettes[i]))(length(categories)), length(categories))
        }
        
        names(colcolors) <- levels(sample_annotation[, i])
        
        colors[[colnames(sample_annotation)[i]]] <- colcolors
    }
    colors
} 
