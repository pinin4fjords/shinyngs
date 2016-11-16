#' The input function of the heatmap module
#' 
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion 
#' heatmaps plot expression for samples by column and e.g. genes by row. A 
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
#' 
#' This provides the form elements to control the heatmap display
#' 
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param type The type of heatmap that will be made. 'expression', 'samples' or
#'   'pca'
#'   
#' @return output An HTML tag object that can be rendered as HTML using 
#'   as.character()
#'   
#' @keywords shiny
#'   
#' @examples
#' heatmapInput('heatmap', ese, group_vars, default_groupvar)
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('heatmap', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

heatmapInput <- function(id, eselist, type = "expression") {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("heatmap"), eselist)
    
    # Only provide controls for clustering etc for the expression heat maps
    
    if (type == "expression") {
        heatmap_filters <- list(h5("Clustering"), checkboxInput(ns("cluster_rows"), "Cluster rows?", TRUE), checkboxInput(ns("cluster_cols"), "Cluster columns?", 
            FALSE), radioButtons(ns("scale"), "Scale by:", c(Row = "row", Column = "column", None = "none")))
    } else {
        if (type == "pca") {
            cluster_rows <- TRUE
            cluster_cols <- FALSE
        } else {
            cluster_rows <- TRUE
            cluster_cols <- TRUE
        }
        heatmap_filters <- list(hiddenInput(ns("cluster_rows"), cluster_rows), hiddenInput(ns("cluster_cols"), cluster_cols), hiddenInput(ns("scale"), "none"))
    }
    
    interactivity_filter <- radioButtons(ns("interactive"), "Interactivity", c(interactive = TRUE, annotated = FALSE))
    
    # Output sets of fields in their own containers
    
    if (type == "pca" && length(eselist@group_vars) == 0) {
        filters <- list(interactivity_filter, groupbyInput(ns("heatmap"), color = FALSE), heatmap_filters, fieldSets(ns("fieldset"), list(expression = expression_filters, 
            export = plotdownloadInput(ns("heatmap")))))
    } else {
        filters <- fieldSets(ns("fieldset"), list(heatmap = list(interactivity_filter, groupbyInput(ns("heatmap"), color = FALSE), heatmap_filters), expression = expression_filters, 
            export = plotdownloadInput(ns("heatmap"))))
    }
    
    filters
    
}

#' The output function of the heatmap module
#' 
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion 
#' heatmaps plot expression for samples by column and e.g. genes by row. A 
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
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
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('heatmap', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

heatmapOutput <- function(id, type = "") {
    ns <- NS(id)
    
    # Add in the help modal
    
    help <- list()
    
    if (type == "pca") {
        help = list(modalInput(ns("pcavsexperiment"), "help", "help"), modalOutput(ns("pcavsexperiment"), "Principal components vs experimental variables", 
            includeMarkdown(system.file("inlinehelp", "pcavsexperiment.md", package = packageName()))))
    } else if (type == "samples") {
        help = list(modalInput(ns("clusteringheatmap"), "help", "help"), modalOutput(ns("clusteringheatmap"), "Sample clustering heatmap", includeMarkdown(system.file("inlinehelp", 
            "clusteringheatmap.md", package = packageName()))))
    } else if (type == "expression") {
        help = list(modalInput(ns("expressionheatmap"), "help", "help"), modalOutput(ns("expressionheatmap"), "Expression heatmap", includeMarkdown(system.file("inlinehelp", 
            "expressionheatmap.md", package = packageName()))))
    }
    
    # Return outputs and help link
    
    list(help, uiOutput(ns("heatmap_ui")))
}

#' The server function of the heatmap module
#' 
#' Three types of heatmaps are provided, employed in various places in the
#' rnaseq app (for example), and using much of the same code. Expresssion 
#' heatmaps plot expression for samples by column and e.g. genes by row. A 
#' samples heatmap plots samples vs samples to illustrate correlation patterns.
#' A pca heatmap plots the results of anova tests applied to examine the
#' associations between principal components and experimental variables.
#' 
#' This function is not called directly, but rather via callModule() (see 
#' example).
#' 
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#' @param type The type of heatmap that will be made. 'expression', 'samples' or
#'   'pca' (default: 'expression')
#'   
#' @keywords shiny
#'   
#' @examples
#' callModule(heatmap, 'heatmap', eselist, type = 'pca')
#' 
#' # Almost certainly used via application creation
#' 
#' data(zhangneurons)
#' app <- prepareApp('heatmap', zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)

heatmap <- function(input, output, session, eselist, type = "expression") {
    
    ns <- session$ns
    
    # Make the groupby UI element
    
    unpack.list(callModule(groupby, "heatmap", eselist = eselist, group_label = "Annotate with variables:", multiple = TRUE))
    
    # Call the selectmatrix module and unpack the reactives it sends back
    
    if (type == "expression") {
        unpack.list(callModule(selectmatrix, "heatmap", eselist, var_max = 500))
    } else {
        unpack.list(callModule(selectmatrix, "heatmap", eselist, var_n = 1000))
    }
    
    # Plot interactive / non-interactive version of heatmap dependent on input
    
    output$heatmap_ui <- renderUI({
        if (input$interactive) {
            withProgress(message = "Preparing heatmap container", value = 0, {
                list(h3(makeTitle()), d3heatmap::d3heatmapOutput(ns("interactiveHeatmap"), height = plotHeight()))
            })
        } else {
            list(h4(makeTitle()), plotOutput(ns("annotatedHeatmap")))
        }
    })
    
    # Create a title
    
    makeTitle <- reactive({
        if (type == "pca") {
            paste("PCA vs variable association plot based on expression matrix:", matrixTitle())
        } else if (type == "expression") {
            paste("Expression heat map based on expression matrix:", matrixTitle())
        } else {
            paste("Sample clustering heat map based on expression matrix:", matrixTitle())
        }
    })
    
    # Get the experiment data and tidy up as appropriate
    
    getExperimentData <- reactive({
        if (isSummarised()) {
            NULL
        } else {
            ed <- selectColData()
            
            anno_fields <- getGroupby()
            
            if (!is.null(anno_fields)) {
                
                # Prettify the factor levels for display
                
                colnames(ed)[match(anno_fields, colnames(ed))] <- prettifyVariablename(anno_fields)
                group_vars <- prettifyVariablename(anno_fields)
                
                # Make factors from the specified grouping variables
                
                sm <- selectMatrix()
                ed <- ed[colnames(sm), , drop = FALSE]
                
                ed <- data.frame(lapply(structure(group_vars, names = group_vars), function(x) factor(ed[, x], levels = unique(ed[, x]))), check.names = FALSE, 
                  stringsAsFactors = FALSE, row.names = rownames(ed))
                
                # Order by the group variables for display purposes
                
                ed[do.call(order, as.list(ed[, group_vars, drop = FALSE])), , drop = FALSE]
            } else {
                ed
            }
        }
    })
    
    
    # Get a matrix of annotation to use in the plots. This is the experiment data except when type is 'pca', when it's not relevant
    
    getPlotAnnotation <- reactive({
        if (type == "pca") {
            NULL
        } else {
            getExperimentData()
        }
    })
    
    # Get a a matrix of the values we actually want the user to see in mouseovers etc.
    
    getDisplayMatrix <- reactive({
        pm <- selectMatrix()
        
        if (type == "samples") {
            pm <- cor(pm, use = "complete.obs", method = "spearman")
        } else if (type == "pca") {
            pm <- getPCAMatrix()
        }
        
        # We can't do clustering with anything with the same value in all columns. So take these out.
        
        if (as.logical(input$cluster_rows) && !is.null(getExperimentData())) {
            pm <- pm[apply(pm, 1, function(x) length(unique(x)) > 1), , drop = FALSE]
        }
        
        # For expression, re-order by the experiment
        
        if (type == "expression") {
            pm <- pm[, rownames(getExperimentData())]
        }
        pm
    })
    
    # Create of values to use in plotting, i.e. to define the colors
    
    getPlotMatrix <- reactive({
        pm <- getDisplayMatrix()
        
        if (type == "expression") {
            pm <- log2(pm + 1)
        } else if (type == "pca") {
            pm[pm < 0.001] <- 0.001
            log10(pm[apply(pm, 1, function(x) !all(is.na(x))), ])
        }
        pm
    })
    
    # Run a PCA with the currently selected matrix
    
    getPCAMatrix <- reactive({
        pcameta <- getExperimentData()
        
        # Take out anything with less than 2 unique values or the below won't work
        
        pcameta <- pcameta[, chooseGroupingVariables(pcameta), drop = FALSE]
        
        pcavals <- selectMatrix()[, rownames(pcameta)]
        
        pca <- runPCA(pcavals)
        fraction_explained <- calculatePCAFractionExplained(pca)
        
        # Use 10 components or however many fewer is produced by the PCA
        
        last_pc <- 10
        if (ncol(pca$x) < last_pc) {
            last_pc <- ncol(pca$x)
        }
        
        # Make a blank matrix to hold the p values
        
        pvals <- matrix(data = NA, nrow = ncol(pcameta), ncol = last_pc, dimnames = list(colnames(pcameta), paste(paste("PC", 1:last_pc, sep = ""), " (", fraction_explained[1:last_pc], 
            "%)", sep = "")))
        
        # Fill the matrix with anova p values
        
        for (i in 1:ncol(pcameta)) {
            for (j in 1:last_pc) {
                fit <- aov(pca$x[, j] ~ factor(pcameta[, i]))
                if ("Pr(>F)" %in% names(summary(fit)[[1]])) {
                  pvals[i, j] <- summary(fit)[[1]][["Pr(>F)"]][[1]]
                }
            }
        }
        
        pvals
    })
    
    # Calculate heights for the the various types of heatmap
    
    plotHeight <- reactive({
        (nrow(getDisplayMatrix()) * rowHeight()) + dendroHeight() + annotationsHeight() + xAxisLabelsHeight()
    })
    
    # Add an allowance for the axis labels. Interactive view doesn't have annotations, so we use the labels, so need more space
    
    xAxisLabelsHeight <- reactive({
        if (input$interactive) {
            350
        } else {
            120
        }
    })
    
    # Add a chunk for the dendrogram at the top
    
    dendroHeight <- reactive({
        if (as.logical(input$cluster_cols)) {
            150
        } else {
            0
        }
    })
    
    # Small row height for expression heat map (probably lots of rows)
    
    rowHeight <- reactive({
        if (type == "expression") {
            12
        } else if (type == "pca") {
            50
        } else {
            20
        }
    })
    
    # Annotations allowance for pheatmap
    
    annotationsHeight <- reactive({
        
        if (input$interactive) {
            0
        } else {
            anno_fields <- getGroupby()
            (length(anno_fields) * 14)
        }
    })
    
    # Calculate the plot width based on the inputs
    
    plotWidth <- reactive({
        return(400 + (18 * ncol(getDisplayMatrix())))
    })
    
    # Make row labels
    
    rowLabels <- reactive({
        ese <- getExperiment()
        plot_matrix <- getPlotMatrix()
        
        if (length(ese@labelfield) > 0 && type == "expression") {
            annotation <- as.data.frame(mcols(ese))
            labels <- annotation[match(rownames(plot_matrix), annotation[[ese@idfield]]), ese@labelfield]
            labels[!is.na(labels)] <- paste(labels[!is.na(labels)], rownames(plot_matrix)[!is.na(labels)], sep = " / ")
            labels[is.na(labels)] <- rownames(plot_matrix)[is.na(labels)]
            labels
        } else {
            rownames(plot_matrix)
        }
    })
    
    # Make a color palette
    
    makeColors <- reactive({
        colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(100)
        
        if (type == "pca") {
            colors <- rev(colors)
        }
        
        colors
    })
    
    # Font sizes for interactive plot
    
    cexRow <- reactive({
        if (type == "pca") {
            1
        } else {
            0.7
        }
    })
    
    cexCol <- reactive({
        if (type == "pca") {
            1
        } else {
            0.7
        }
    })
    
    displayNumbers <- reactive({
        if (type == "pca") {
            TRUE
        } else {
            FALSE
        }
    })
    
    # Make an interactive heatmap version
    
    output$interactiveHeatmap <- d3heatmap::renderD3heatmap({
        withProgress(message = "Building interactive heatmap", value = 0, {
            interactiveHeatmap(plotmatrix = getPlotMatrix(), displaymatrix = getDisplayMatrix(), getPlotAnnotation(), cluster_cols = as.logical(input$cluster_cols), 
                cluster_rows = as.logical(input$cluster_rows), scale = input$scale, row_labels = rowLabels(), colors = makeColors(), cexCol = cexCol(), cexRow = cexRow())
        })
    })
    
    # Make a static heatmap version - with the benefit of column annotations in pheatmap
    
    output$annotatedHeatmap <- renderPlot({
        withProgress(message = "Building static heatmap", value = 0, {
            annotatedHeatmap(plotmatrix = getPlotMatrix(), displaymatrix = getDisplayMatrix(), getPlotAnnotation(), cluster_cols = as.logical(input$cluster_cols), 
                cluster_rows = as.logical(input$cluster_rows), scale = input$scale, row_labels = rowLabels(), row_height = rowHeight(), colors = makeColors(), 
                display_numbers = displayNumbers())
        })
    }, height = plotHeight)
    
    # The same function call as static for providing the download
    
    plotHeatmap <- reactive({
        annotatedHeatmap(plotmatrix = getPlotMatrix(), displaymatrix = getDisplayMatrix(), getExperimentData(), cluster_cols = as.logical(input$cluster_cols), 
            cluster_rows = as.logical(input$cluster_rows), scale = input$scale, row_labels = rowLabels(), row_height = rowHeight(), colors = makeColors())
    })
    
    # Call to plotdownload module
    
    observe({
        callModule(plotdownload, "heatmap", makePlot = plotHeatmap, filename = "heatmap.png", plotHeight = plotHeight, plotWidth = plotWidth)
    })
    
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
#' @param displaymatrix A matrix of values that might be displayed in cells
#' @param cluster_cols Cluster columns?
#' @param cluster_rows Cluster rows?
#' @param scale 'row', 'column' or none 
#' @param row_labels Vector labels to use for rows
#' @param row_height The height to use for each row
#'
#' @return output A plot as produced by pheatmap() 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

annotatedHeatmap <- function(plotmatrix, displaymatrix, sample_annotation, cluster_cols, cluster_rows, scale, row_labels, row_height = 12, colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, 
    name = "RdYlBu")))(100), display_numbers = FALSE) {
    
    rownames(plotmatrix) <- row_labels
    
    annotation <- annotation_colors <- NA
    if (!is.null(sample_annotation)) {
        annotation <- sample_annotation
        annotation_colors <- makeAnnotationColors(annotation)
    }
    
    # Turn off scaling if there's only 2 possible values in the matrix, otherwise things look a bit odd
    
    if (length(unique(as.numeric(plotmatrix))) < 3) {
        scale <- "none"
    }
    
    pheatmap::pheatmap(plotmatrix, show_rownames = T, fontsize = 12, fontsize_row = 10, cellheight = row_height, annotation_col = annotation, annotation_colors = annotation_colors, 
        border_color = NA, legend = FALSE, cluster_cols = cluster_cols, cluster_rows = cluster_rows, clustering_distance_rows = calculateDist(t(plotmatrix)), 
        clustering_distance_cols = calculateDist(plotmatrix), clustering_method = "ward.D2", treeheight_col = 150, scale = scale, color = colors, display_numbers = display_numbers, 
        number_color = "white", fontsize_number = 14)
}

#' Make a ineractive heatmap with d3heatmap
#' 
#' This is a generic function which may be useful outside of this package. It
#' produces a heatmap based on an expression matrix and accompanying 
#' experiment data in the form of a frame, \code{d3heatmap()}.
#'
#' @param plotmatrix Expression/ other data matrix
#' @param displaymatrix A matrix of values that might be displayed in cells
#' @param cluster_cols Cluster columns?
#' @param cluster_rows Cluster rows?
#' @param scale 'row', 'column' or none 
#' @param row_labels Vector labels to use for rows
#' @param row_height The height to use for each row
#'
#' @return output A plot as produced by pheatmap() 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

interactiveHeatmap <- function(plotmatrix, displaymatrix, sample_annotation, cluster_rows = TRUE, cluster_cols = FALSE, scale = "row", row_labels, colors = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, 
    name = "RdYlBu")))(100), cexCol = 0.7, cexRow = 0.7, ...) {
    
    # should be possible to specify this in the labRow parameter- but the clustering messes it up
    
    rownames(plotmatrix) <- row_labels
    
    dendrogram <- "none"
    Rowv <- FALSE
    Colv <- FALSE
    
    if ((!is.null(sample_annotation)) && ncol(sample_annotation) > 0) {
        colnames(plotmatrix) <- paste0(colnames(plotmatrix), " (", apply(sample_annotation[colnames(plotmatrix), colnames(sample_annotation), drop = FALSE], 
            1, function(x) paste(x, collapse = "-")), ")")
    }
    
    if (nrow(plotmatrix) < 2) {
        cluster_rows <- FALSE
    }
    
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
    
    # This next is to match with pheatmap
    
    if (class(Rowv) == "dendrogram") {
        Rowv <- rev(Rowv)
    }
    
    # If this is a samples/samples heatmap, reverse the dendrograms to match with pheatmap behaviour
    
    yaxis_width = max(unlist(lapply(rownames(plotmatrix), function(x) nchar(x)))) * (cexRow * 15)
    # xaxis_height = max(unlist(lapply(colnames(plotmatrix), function(x) nchar(x)))) * 10
    xaxis_height = 300
    
    # Turn off scaling if there's only 2 possible values in the matrix, otherwise things look a bit odd
    
    if (length(unique(as.numeric(plotmatrix))) < 3) {
        scale <- "none"
    }
    
    d3heatmap::d3heatmap(plotmatrix, dendrogram = dendrogram, cellnote = displaymatrix, Rowv = Rowv, Colv = Colv, scale = scale, xaxis_height = xaxis_height, 
        yaxis_width = yaxis_width, colors = colors, cexCol = cexCol, cexRow = cexRow, revC = FALSE, labRow = row_labels, ...)
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
