#' The input function of the heatmap module
#' 
#' This provides the form elements to control the heatmap display
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
#' heatmapInput('heatmap', se, group_vars, default_groupvar

heatmapInput <- function(id, ses) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("heatmap"), ses)
    heatmap_filters <- list(h5("Clustering"), checkboxInput(ns("cluster_rows"), "Cluster rows?", TRUE), checkboxInput(ns("cluster_cols"), "Cluster columns?", FALSE), 
        radioButtons(ns("scale"), "Scale by:", c(Row = "row", Column = "column", None = "none")), uiOutput(ns("groupVars")))
    
    # Output sets of fields in their own containers
    
    fieldSets(ns("fieldset"), list(heatmap = heatmap_filters, expression = expression_filters, export = plotdownloadInput(ns("heatmap"))))
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
    
    tabsetPanel(tabPanel("Static (with column annotation)", plotOutput(ns("heatMap"))), tabPanel("Interactive (but no column annotation)", uiOutput(ns("interactiveHeatmap_ui"))))
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
#' @param ses List of structuredExperiment objects with assay and experimental
#' data, with additional information in the metadata() slot
#'
#' @keywords shiny
#' 
#' @examples
#' callModule(heatmap, 'heatmap', se, params$idfield, params$entrezgenefield, params$labelfield, geneset_files=params$geneset_files)

heatmap <- function(input, output, session, ses) {
    
    vals <- reactiveValues(hmfield = 0)
    
    ns <- session$ns
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    unpack.list(callModule(selectmatrix, "heatmap", ses, var_max = 500))
    
    # Populate the groupVars UI element if applicable
    
    output$groupVars <- renderUI({
        se <- getExperiment()
        
        if ("group_vars" %in% names(metadata(se))) {
            checkboxGroupInput(ns("groupVars"), "Annotate with variables:", metadata(se)$group_vars, selected = metadata(se)$group_vars, inline = TRUE)
        }
    })
    
    # Select out the expression values we need for a heatmap
    
    getHeatmapExpression <- reactive({
        
        se <- getExperiment()
        
        withProgress(message = "Getting values for heatmap", value = 0, {
            
            heatmap_expression <- selectMatrix()
            
            if (nrow(heatmap_expression) > 0) {
                
                # Name the rows by gene if we know what the necessary annotation fields are
                
                if (all(c("idfield", "labelfield") %in% names(metadata(se))) && nrow(mcols(se)) > 0) {
                  annotation <- data.frame(mcols(se))
                  display_genes <- annotation[match(rownames(heatmap_expression), annotation[[metadata(se)$idfield]]), metadata(se)$labelfield]
                  hasgenes <- !is.na(display_genes)
                  
                  rownames(heatmap_expression)[hasgenes] <- paste(display_genes[hasgenes], rownames(heatmap_expression)[hasgenes], sep = " / ")
                }
                return(heatmap_expression)
            } else {
                return(NULL)
            }
        })
    })
    
    # Calculate the plot height based on the inputs
    
    plotHeight <- reactive({
        
        # validate(need(!is.null(input$geneSelect), 'Waiting for form to provide geneSelect'))
        
        nrows <- 0
        titleheight = 25
        labelsheight = 80
        dendro_height = 0
        
        # It's not actually possible to set the dendrogram height in d3heatmap, so we're just adding an aribrary value
        
        if (input$cluster_cols) {
            dendro_height = 150
        }
        
        return((titleheight * nlines(matrixTitle())) + (length(input$groupVars) * 14) + (nrow(selectMatrix()) * 12) + labelsheight + dendro_height)
    })
    
    # Calculate the height for the interactive heatmap
    
    interactiveHeight <- reactive({
        xaxis_height = 300
        rowheight = 12
        dendro_height = 0
        
        # It's not actually possible to set the dendrogram height in d3heatmap, so we're just adding an aribrary value
        
        if (input$cluster_cols) {
            dendro_height = 150
        }
        
        return(dendro_height + (nrow(selectMatrix()) * rowheight) + xaxis_height)
    })
    
    # Calculate the plot width based on the inputs
    
    plotWidth <- reactive({
        return(400 + (18 * ncol(getHeatmapExpression())))
    })
    
    # Want heatmap to know if the data is summarised so it doesn't try to annotate the plot
    
    experimentData <- reactive({
        if (isSummarised()) {
            NULL
        } else {
            selectColData()
        }
    })
    
    # Supply a rendered heatmap for display
    
    output$heatMap <- renderPlot({
        makeHeatmap(input, getHeatmapExpression(), experimentData(), main = matrixTitle())
    }, height = plotHeight)
    
    
    # Provide the heatmap for download with the plotdownload module
    
    plotHeatmap <- reactive({
        makeHeatmap(input, getHeatmapExpression(), experimentData(), main = matrixTitle())
    })
    
    # Call to plotdownload module
    
    callModule(plotdownload, "heatmap", makePlot = plotHeatmap, filename = "heatmap.png", plotHeight = plotHeight, plotWidth = plotWidth)
    
    # Add an interactive version of the heatmap
    
    output$interactiveHeatmap_ui <- renderUI({
      withProgress(message = "Building interactive heatmap", value = 0, {
        list(h4(matrixTitle()), d3heatmapOutput(ns("interactiveHeatmap"), height = interactiveHeight()))
      })
    })
    
    # Make an interactive heatmap version
    
    output$interactiveHeatmap <- renderD3heatmap({
        makeHeatmap(input, getHeatmapExpression(), experimentData(), main = matrixTitle(), interactive = TRUE)
    })
    
}

#' Make the heatmap
#' 
#' Just an adapter to annotatedHeatmap(), which is designed to be generically
#' useful
#'
#' @param input Input object
#' @param exprmatrix An expression matrix
#' @param experiment A data frame containing experimental variables
#' @param ... Arguments that should be passed through \code{annotatedHeatmap()}
#' to \code{pheatmap()}
#'
#' @return output Heatmap plot 
#' 
#' @examples
#' makeHeatmap(input, getHeatmapExpression(), selectColData(), main = matrixTitle())

makeHeatmap <- function(input, exprmatrix, experiment, interactive = FALSE, ...) {
    
    if (!is.null(exprmatrix)) {
        
        # We can't do clustering with anything with the same value in all columns. So take these out.
        
        if (input$cluster_rows) {
            exprmatrix <- exprmatrix[apply(exprmatrix, 1, function(x) length(unique(x)) > 1), , drop = FALSE]
        }
        
        if (nrow(exprmatrix) > 0) {
            
            # If the above 'if' reduced the frame to 1 row we can't do clustering anyway
            
            cluster_rows <- input$cluster_rows
            if (nrow(exprmatrix) == 1) {
                cluster_rows = FALSE
            }
            
            groupVars <- NULL
            if ("groupVars" %in% names(input)) {
                groupVars <- input$groupVars
            }
            
            annotatedHeatmap(log2(exprmatrix + 1), experiment, group_vars = groupVars, cluster_rows = cluster_rows, cluster_cols = input$cluster_cols, scale = input$scale, 
                interactive = interactive, ...)
            
        }
    }
}

#' Make a heatmap with annotations by experimental variable
#' 
#' This is a generic function which may be useful outside of this package. It
#' produces a heatmap based on an expression matrix and accompanying 
#' experiment data in the form of a frame, using \code{pheatmap()} or 
#' \code{d3heatmap()}.
#' 
#' The clustering parameters for \code{pheatmap()} and \code{d3heatmap()} are 
#' set to be consistent with one another.
#'
#' @param plotmatrix Expression/ other data matrix
#' @param sample_annotation Annotation for the columns of plotmatrix
#' @param group_vars Grouping variables to be used from 
#' sample_annotation
#' @param interactive Make an interactive plot with \code{d3heatmap()} instead
#'of using \code{pheatmap()}? (Default: FALSE)
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

annotatedHeatmap <- function(plotmatrix, sample_annotation, group_vars = NULL, interactive = FALSE, ...) {
    
    if (!is.null(plotmatrix)) {
        
        annotation <- annotation_colors <- NA
        
        if ((!is.null(group_vars)) && !is.null(sample_annotation)) {
            
            # Prettify the factor levels for display
            
            colnames(sample_annotation)[colnames(sample_annotation) %in% group_vars] <- prettifyVariablename(colnames(sample_annotation)[colnames(sample_annotation) %in% 
                group_vars])
            group_vars <- prettifyVariablename(group_vars)
            
            # Make factors from the specified grouping variables
            
            annotation <- data.frame(apply(sample_annotation[colnames(plotmatrix), rev(group_vars), drop = F], 2, as.factor), check.names = FALSE)
            
            # Order by the group variables for display purposes
            
            annotation <- annotation[do.call(order, as.list(annotation[, group_vars, drop = FALSE])), , drop = FALSE]
            plotmatrix <- plotmatrix[, rownames(annotation)]
            
            annotation_colors <- makeAnnotationColors(annotation)
        }
        
        if (interactive) {
            interactiveHeatmap(plotmatrix, annotation, group_vars = group_vars, ...)
            
        } else {
            pheatmap::pheatmap(plotmatrix, show_rownames = T, fontsize = 12, fontsize_row = 10, cellheight = 12, annotation = annotation, annotation_colors = annotation_colors, 
                border_color = NA, legend = FALSE, clustering_distance_rows = calculateDist(t(plotmatrix)), clustering_distance_cols = calculateDist(plotmatrix), 
                clustering_method = "ward.D2", treeheight_col = 150, ...)
        }
    }
}

#' Make a ineractive heatmap with d3heatmap
#' 
#' This is a generic function which may be useful outside of this package. It
#' produces a heatmap based on an expression matrix and accompanying 
#' experiment data in the form of a frame, \code{d3heatmap()}.
#'
#' @param plotmatrix Expression/ other data matrix
#' @param sample_annotation Annotation for the columns of plotmatrix
#' @param group_vars Grouping variables to be used from 
#' sample_annotation
#' @param cluster_rows Make row-wise dendrogram?
#' @param cluster_cols Make column-wise dendrogram?
#' @param scale row, column or none
#'
#' @return output A plot as produced by pheatmap() 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

interactiveHeatmap <- function(plotmatrix, sample_annotation, group_vars, cluster_rows = TRUE, cluster_cols = FALSE, scale = "row", ...) {
    
    dendrogram <- "none"
    Rowv <- FALSE
    Colv <- FALSE
    
    # Specify how the dendrogram should be created
    
    if (all(cluster_rows, cluster_cols)) {
        dendrogram <- "both"
        Rowv <- calculateDendrogram(t(plotmatrix))
        Colv <- calculateDendrogram(plotmatrix)
    } else if (cluster_rows) {
        dendrogram <- "row"
        Rowv <- calculateDendrogram(t(plotmatrix))
    } else if (cluster_cols) {
        dendrogram <- "col"
        Colv <- calculateDendrogram(plotmatrix)
    }
    
    colnames(plotmatrix) <- paste0(colnames(plotmatrix), " (", sample_annotation[colnames(plotmatrix), group_vars[1]], ")")
    
    yaxis_width = max(unlist(lapply(rownames(plotmatrix), function(x) nchar(x)))) * 8
    # xaxis_height = max(unlist(lapply(colnames(plotmatrix), function(x) nchar(x)))) * 10
    xaxis_height = 300
    
    d3heatmap::d3heatmap(plotmatrix, dendrogram = dendrogram, Rowv = Rowv, Colv = Colv, scale = scale, xaxis_height = xaxis_height, yaxis_width = yaxis_width, colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, 
        name = "RdYlBu")))(100), cexCol = 0.8, cexRow = 0.7)
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
            colcolors <- sample(colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[palettes[i], "maxcolors"], palettes[i]))(length(categories)), 
                length(categories))
        }
        
        names(colcolors) <- levels(sample_annotation[, i])
        
        colors[[colnames(sample_annotation)[i]]] <- colcolors
    }
    colors
} 
