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
    
    default_type <- "boxes"
    if (ncol(eselist[[1]]) > 20) {
        default_type <- "lines"
    }
    
    expression_filters <- selectmatrixInput(ns("sampleBoxplot"), eselist)
    quartile_plot_filters <- list(radioButtons(ns("plotType"), "Plot type", c("boxes", "lines"), selected = default_type), numericInput(ns("whiskerDistance"), 
        "Whisker distance in multiples of IQR", value = 1.5), groupbyInput(ns("boxplot")))
    
    field_sets = list()
    naked_fields = list()  # Things we don't want to wrap in a field set - probably hidden stuff
    
    # Don't create an empty field set if we're not grouping
    
    if (length(eselist@group_vars) > 0) {
        field_sets$quartile_plot_filters <- quartile_plot_filters
    } else {
        naked_fields[[1]] <- quartile_plot_filters
    }
    
    field_sets <- c(field_sets, list(expression = expression_filters, export = plotdownloadInput(ns("boxplot"), "box plot")))
    
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
    list(modalInput(ns("boxplot"), "help", "help"), modalOutput(ns("boxplot"), "Quartile plots", includeMarkdown(system.file("inlinehelp", "boxplot.md", 
        package = packageName()))), h3("Quartile plots"), uiOutput(ns("quartilesPlot")))
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
    
    output$quartilesPlot <- renderUI({
        ns <- session$ns
        if (input$plotType == "boxes") {
            plotOutput(ns("sampleBoxplot"))
        } else {
            plotlyOutput(ns("quartilesPlotly"), height = "600px")
        }
    })
    
    output$quartilesPlotly <- renderPlotly({
        plotly_quartiles(selectMatrix(), getExperiment(), getAssayMeasure(), whisker_distance = input$whiskerDistance)
    })
    
    output$sampleBoxplot <- renderPlot({
        withProgress(message = "Making sample boxplot", value = 0, {
            ggplot_boxplot(selectMatrix(), selectColData(), colorBy(), expressiontype = , getAssayMeasure(), whisker_distance = input$whiskerDistance)
        })
    }, height = 600)
    
    # output$sampleBoxplot <- renderPlotly({ withProgress(message = 'Making sample boxplot', value = 0, { plotly_boxplot(selectMatrix(),
    # selectColData(), colorBy(), getAssayMeasure()) }) })
    
    # Provide the plot for download
    
    plotSampleBoxplot <- reactive({
        ggplot_boxplot(selectMatrix(), selectColData(), colorBy())
    })
    
    # Call to plotdownload module
    
    callModule(plotdownload, "boxplot", makePlot = plotSampleBoxplot, filename = "boxplot.png", plotHeight = 600, plotWidth = 800)
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

ggplot_boxplot <- function(plotmatrix, experiment, colorby = NULL, expressiontype = "expression", whisker_distance = 1.5) {
    
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
    
    # Make sure name is a factor to 1) stop ggplot re-ordering the axis and 2) stop it interpreting it as numeric
    
    plotdata$name <- factor(plotdata$name, levels = unique(plotdata$name))
    
    if (!is.null(colorby)) {
        p <- ggplot(plotdata, aes(name, log2_count, fill = colorby)) + geom_boxplot(coef = whisker_distance) + scale_fill_discrete(name = colorby) + 
            guides(fill = guide_legend(nrow = ceiling(length(unique(experiment[[colorby]]))/2)))
    } else {
        p <- ggplot(plotdata, aes(name, log2_count)) + geom_boxplot()
    }
    
    p <- p + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1.5)), axis.title.x = element_blank(), legend.position = "bottom", 
        axis.text.y = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.2)), title = element_text(size = rel(1.3))) + ylab(splitStringToFixedwidthLines(paste0("log2(", 
        expressiontype, ")"), 15))
    
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

plotly_boxplot <- function(matrix, experiment, colorby, expressiontype = "expression") {
    
    plotdata <- ggplotify(as.matrix(matrix), experiment, colorby)
    plot_ly(plotdata, type = "box", y = log2_count, x = name, color = colorby, evaluate = TRUE) %>% layout(yaxis = list(title = expressiontype), 
        xaxis = list(title = NULL), evaluate = TRUE) %>% config(showLink = TRUE)
}

#' Make a line-based alternative to boxplots
#' 
#' Box-plots become unmanagable with large numbers of samples. This function
#' plots lines at the median, quartiles, and whiskers, plotting points for 
#' outliers beyond that
#'
#' @param matrix 
#' @param ese ExploratorySummarizedExperiment
#' @param expressiontype Y axis label
#' @param whisker_distance IQR multiplier for whiskers, and beyond which to 
#' show outliers
#'
#' @export

plotly_quartiles <- function(matrix, ese, expressiontype = "expression", whisker_distance = 1.5) {
    matrix <- log2(matrix+1)
    
    quantiles <- apply(matrix, 2, quantile, na.rm = TRUE)
    samples <- structure(colnames(matrix), names = colnames(matrix))
    iqrs <- lapply(samples, function(x) {
        quantiles["75%", x] - quantiles["25%", x]
    })
    
    outliers <- lapply(samples, function(x) {
        y <- matrix[, x]
        ol <- y[which(y > quantiles["75%", x] + iqrs[[x]] * whisker_distance | y < quantiles["25%", x] - iqrs[[x]] * whisker_distance)]
        if (length(ol) > 0) {
            data.frame(x = x, y = ol, label = idToLabel(names(ol), ese), stringsAsFactors = FALSE)
        } else {
            NULL
        }
    })
    outliers <- do.call(rbind, outliers[!unlist(lapply(outliers, is.null))])
    
    # These lines to force plotly to use and display sample IDs as strings. For some reason character strings of numeric things get 
    # converted back
    
    samples <- paste0(samples, '&nbsp;')
    outliers$x <- paste0(outliers$x, '&nbsp;')
    
    # The polotting business
    
    plot_ly(type = "line", showlegend = FALSE, evaluate = TRUE) %>% add_trace(x = samples, y = quantiles["50%", ], group = "median", mode = "lines", 
        line = list(color = "black"), showlegend = TRUE, evaluate = TRUE) %>% add_trace(x = samples, y = quantiles["25%", ], group = "quartiles", 
        mode = "lines", line = list(dash = "dash", color = "black"), showlegend = TRUE, evaluate = TRUE) %>% add_trace(x = samples, y = quantiles["75%", 
        ], group = "quartiles", mode = "lines", line = list(dash = "dash", color = "black"), evaluate = TRUE) %>% add_trace(x = samples, y = quantiles["75%", 
        ] + ((quantiles["75%", ] - quantiles["25%", ]) * whisker_distance), mode = "lines", group = paste0("quartiles<br />+/- (IQR * ", whisker_distance, 
        ")"), line = list(width = 1, color = "grey"), showlegend = TRUE, evaluate = TRUE) %>% add_trace(x = samples, y = quantiles["25%", ] - ((quantiles["75%", 
        ] - quantiles["25%", ]) * whisker_distance), mode = "lines", line = list(width = 1, color = "grey"), group = paste0("quartiles<br />+/- (IQR * ", 
        whisker_distance, ")"), evaluate = TRUE) %>% add_trace(x = outliers$x, y = outliers$y, mode = "markers", name = "outliers", marker = list(color = "black"), 
        showlegend = TRUE, evaluate = TRUE, hoverinfo = "text", text = outliers$label) %>% layout(xaxis = list(title = NULL), yaxis = list(title = paste0("log2(", 
        expressiontype, ")")), margin = list(b = 150), hovermode = "closest", title = NULL, evaluate = TRUE)
    
}

 
