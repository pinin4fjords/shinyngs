#' The input function of the boxplot module
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
#' boxplotInput(ns('boxplot'), ses)

boxplotInput <- function(id, ses) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("sampleBoxplot"), ses)
    
    boxplot_filters <- uiOutput(ns("colorBy"))
    
    fieldSets(ns("fieldset"), list(boxplot = boxplot_filters, expression = expression_filters, export = plotdownloadInput(ns("boxplot"))))
    
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
#' boxplotOutput('boxplot')

boxplotOutput <- function(id) {
    ns <- NS(id)
    plotOutput(ns("sampleBoxplot"))
}

#' The server function of the boxplot module
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
#' callModule(boxplot, 'boxplot', ses)

boxplot <- function(input, output, session, ses) {
    
    # Get the expression matrix - no need for a gene selection
    
    selectmatrix_functions <- callModule(selectmatrix, "sampleBoxplot", ses, select_genes = FALSE)
    
    selectMatrix <- selectmatrix_functions$selectMatrix
    matrixTitle <- selectmatrix_functions$title
    selectColData <- selectmatrix_functions$selectColData
    getExperiment <- selectmatrix_functions$getExperiment
    
    # Call to plotdownload module
    
    callModule(plotdownload, "boxplot", makePlot = makeSampleBoxPlot, filename = "boxplot.png", plotHeight = 400, plotWidth = 600)
    
    output$colorBy <- renderUI({
        ns <- session$ns
        se <- getExperiment()
        if ("group_vars" %in% names(metadata(se))) {
            selectInput(ns("colorBy"), "Color by", metadata(se)$group_vars, selected = metadata(se)$default_groupvar)
        }
    })
    
    colorBy <- reactive({
        if ("colorBy" %in% names(input)) {
            return(input$colorBy)
        } else {
            return(NULL)
        }
    })
    
    makeSampleBoxPlot <- reactive({
        validate(need(!is.null(input$colorBy), "Waiting for form to provide colorBy"))
        ggplot_boxplot(selectMatrix(), selectColData(), colorBy())
    })
    
    output$sampleBoxplot <- renderPlot({
        withProgress(message = "Making sample boxplot", value = 0, {
            
            makeSampleBoxPlot()
        })
    }, height = 500)
}

#' Make a boxplot with coloring by experimental variable
#' 
#' A simple function using \code{ggplot2} to make a sample boxplot
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param expressiontype Expression type for use in y axis label
#'
#' @return output A \code{ggplot} output
#'
#' @keywords keywords
#'
#' @import ggplot2
#' @export
#' 
#' @examples
#' ggplot_boxplot(selectMatrix(), selectColData(), colorBy())

ggplot_boxplot <- function(plotmatrix, experiment, colorby = NULL, expressiontype = "Normalised counts per million") {
    
    # If color grouping is specified, sort by the coloring variable so the groups will be plotted together
    
    if (!is.null(colorby)) {
        experiment <- experiment[order(experiment[[colorby]]), ]
        plotmatrix <- plotmatrix[, rownames(experiment)]
    }
    
    # Reshape the data for ggplot2
    
    plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)
    
    if (!is.null(colorby)) {
        p <- ggplot(plotdata, aes(name, log2_count, fill = colorby)) + geom_boxplot() + scale_fill_discrete(name = colorby) + guides(fill = guide_legend(nrow = ceiling(length(unique(experiment[[colorby]]))/3)))
    } else {
        p <- ggplot(plotdata, aes(name, log2_count)) + geom_boxplot()
    }
    
    p <- p + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.8)), axis.title.x = element_blank(), legend.position = "bottom", axis.text.y = element_text(size = rel(1.8)), legend.text = element_text(size = rel(1.8)), 
        title = element_text(size = rel(2))) + ylab(splitStringToFixedwidthLines(paste0("log2(", expressiontype, ")"), 15))
    
    print(p)
}

#' Make a boxplot with coloring by experimental variable
#' 
#' A simple function using \code{plotly} to make a sample boxplot.
#' NOT CURRENTLY USED DUE TO RESOURCE REQUIREMENTS ON LARGE MATRICES
#'
#' @param plotmatrix Expression/ other data matrix
#' @param experiment Annotation for the columns of plotmatrix
#' @param colorby Column name in \code{experiment} specifying how boxes should be colored
#' @param expressiontype Expression type for use in y axis label
#'
#' @return output A \code{plotly} output
#'
#' @keywords keywords
#'
#' @import plotly

plotly_boxplot <- function(matrix, experiment, colorby, expressiontype = "Normalised counts per million") {
    
    plotdata <- ggplotify(as.matrix(matrix), experiment, colorby)
    plot_ly(plotdata, type = "box", y = log2_count, x = name, color = colorby)
} 
