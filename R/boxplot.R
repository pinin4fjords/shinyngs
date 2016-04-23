#' The input function of the boxplot module
#' 
#' This provides the form elements to control the pca display
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
#' boxplotInput(ns('boxplot'), eselist)

boxplotInput <- function(id, eselist) {
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("sampleBoxplot"), eselist)
    boxplot_filters <- groupbyInput(ns("boxplot"))
    
    field_sets = list()
    naked_fields = list()  # Things we don't want to wrap in a field set - probably hidden stuff
    
    # Don't create an empty field set if we're not grouping
    
    if (length(eselist@group_vars) > 0) {
        field_sets$boxplot_filters <- boxplot_filters
    } else {
        naked_fields[[1]] <- boxplot_filters
    }
    
    # field_sets <- c(field_sets, list(expression = expression_filters, export = plotdownloadInput(ns('boxplot'))))
    field_sets <- c(field_sets, list(expression = expression_filters))
    
    list(naked_fields, fieldSets(ns("fieldset"), field_sets))
    
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
    list(h3("Box plots"), plotlyOutput(ns("sampleBoxplot")))
}

#' The server function of the boxplot module
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
#' callModule(boxplot, 'boxplot', eselist)

boxplot <- function(input, output, session, eselist) {
    
    # Get the expression matrix - no need for a gene selection
    
    unpack.list(callModule(selectmatrix, "sampleBoxplot", eselist, select_genes = FALSE))
    
    colorBy <- callModule(groupby, "boxplot", eselist = eselist, group_label = "Color by")
    
    # Render the plot
    
    # output$sampleBoxplot <- renderPlot({ withProgress(message = 'Making sample boxplot', value = 0, { ggplot_boxplot(selectMatrix(), selectColData(), colorBy()) })
    # }, height = 600)
    
    output$sampleBoxplot <- renderPlotly({
        withProgress(message = "Making sample boxplot", value = 0, {
            plotly_boxplot(selectMatrix(), selectColData(), colorBy(), getAssayMeasure())
        })
    })
    
    # Provide the plot for download
    
    # plotSampleBoxplot <- reactive({ ggplot_boxplot(selectMatrix(), selectColData(), colorBy()) })
    
    # Call to plotdownload module
    
    # callModule(plotdownload, 'boxplot', makePlot = plotSampleBoxplot, filename = 'boxplot.png', plotHeight = 600, plotWidth = 800)
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
        colnames(experiment)[colnames(experiment) == colorby] <- prettifyVariablename(colorby)
        colorby <- prettifyVariablename(colorby)
        
        experiment <- experiment[order(experiment[[colorby]]), ]
        experiment[[colorby]] <- na.replace(experiment[[colorby]], "N/A")
        plotmatrix <- plotmatrix[, rownames(experiment)]
    }
    
    # Reshape the data for ggplot2
    
    plotdata <- ggplotify(as.matrix(plotmatrix), experiment, colorby)
    
    if (!is.null(colorby)) {
        p <- ggplot(plotdata, aes(name, log2_count, fill = colorby)) + geom_boxplot() + scale_fill_discrete(name = colorby) + guides(fill = guide_legend(nrow = ceiling(length(unique(experiment[[colorby]]))/2)))
    } else {
        p <- ggplot(plotdata, aes(name, log2_count)) + geom_boxplot()
    }
    
    p <- p + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)), axis.title.x = element_blank(), legend.position = "bottom", axis.text.y = element_text(size = rel(1.5)), 
        legend.text = element_text(size = rel(1.2)), title = element_text(size = rel(1.3))) + ylab(splitStringToFixedwidthLines(paste0("log2(", expressiontype, ")"), 
        15))
    
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

plotly_boxplot <- function(matrix, experiment, colorby, expressiontype = "expression") {
    
    plotdata <- ggplotify(as.matrix(matrix), experiment, colorby)
    plot_ly(plotdata, type = "box", y = log2_count, x = name, color = colorby, evaluate = TRUE) %>% layout(yaxis = list(title = expressiontype), xaxis = list(title = NULL), 
        evaluate = TRUE) %>% config(showLink = TRUE)
} 
