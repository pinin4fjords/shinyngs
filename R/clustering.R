#' The input function of the clustering module
#' 
#' This module plots the expression profiles (scaled for comparison) of the 
#' selected rows of the input matrix provided by the \code{selectmatrix} module.
#' 
#' The \code{\link[clustering]{clara}} method, a fast approximation of 
#' partitioning about medoids, is used to produce the clusters. As well as 
#' defining the input matrix users can decide how the clusters are drawn and how
#' many clusters should be generated.
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
#' clusteringInput('myid', zhangneurons)

clusteringInput <- function(id, eselist) {
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("clustering"), eselist)
    
    cluster_displays <- c(`Sample lines` = "sample_lines", `Error bars` = "error_bars", `Filled line` = "filled_line")
    
    cluster_filters <- list(selectInput(ns("cluster_number"), label = "Number of clusters", choices = 1:12, selected = 6), selectInput(ns("cluster_display"), 
        label = "Cluster display", choices = cluster_displays, selected = "filled_line"), colormakerInput(ns("clustering")), selectInput(ns("average_type"), 
        label = "Average type", choices = c("mean", "median"), selected = "mean"), selectInput(ns("limits"), label = "Limits show", choices = c(`Standard deviation` = "sd", 
        `Standard error` = "se", `95% confidence interval` = "ci"), selected = "sd"))
    
    # Ad clustering filters to clustering and table export fields
    
    fieldSets(ns("fieldset"), list(clustering = cluster_filters, expression = expression_filters, export = simpletableInput(ns("geneClusteringTable"))))
}


#' The output function of the clustering module
#' 
#' This module plots the expression profiles (scaled for comparison) of the 
#' selected rows of the input matrix provided by the \code{selectmatrix} module.
#' 
#' The \code{\link[clustering]{clara}} method, a fast approximation of 
#' partitioning about medoids, is used to produce the clusters. As well as 
#' defining the input matrix users can decide how the clusters are drawn and how
#' many clusters should be generated.
#' 
#' This funcion provides the form elements to control the display
#' 
#' This provides actual plot element for display by applications
#' 
#' @param id Submodule namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using 
#' as.character() 
#'
#' @keywords shiny
#' 
#' @examples
#' clusteringOutput('myid')

clusteringOutput <- function(id) {
    ns <- NS(id)
    list(modalInput(ns("clustering"), "help", "help"), modalOutput(ns("clustering"), "Feature-wise clustering", includeMarkdown(system.file("inlinehelp", "clustering.md", 
        package = packageName()))), uiOutput(ns("geneClusteringTitle")), plotlyOutput(ns("geneClusteringPlot"), height = 600), h4("Table of values by cluster"), 
        simpletableOutput(ns("geneClusteringTable")))
}

#' The server function of the clustering module
#' 
#' This module plots the expression profiles (scaled for comparison) of the 
#' selected rows of the input matrix provided by the \code{selectmatrix} module.
#' 
#' The \code{\link[clustering]{clara}} method, a fast approximation of 
#' partitioning about medoids, is used to produce the clusters. As well as 
#' defining the input matrix users can decide how the clusters are drawn and how
#' many clusters should be generated.
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
#' callModule(clustering, 'myid', eselist)

clustering <- function(input, output, session, eselist, ncolors) {
    
    # Call the selectmatrix module to get the expression matrix - no need for a gene selection
    
    unpack.list(callModule(selectmatrix, "clustering", eselist, select_genes = TRUE, var_n = 1000, provide_all_genes = TRUE, default_gene_select = "variance"))
    
    getPalette <- callModule(colormaker, "clustering", getNumberCategories = getClusterNumber)
    
    ############################################################################# Form accessors
    
    # The number of clusters
    
    getClusterNumber <- reactive({
        validate(need(!is.null(input$cluster_number), "waiting for cluster number"))
        as.numeric(input$cluster_number)
    })
    
    # How limits of error bars etc should be defined
    
    getLimits <- reactive({
        validate(need(!is.null(input$limits), "Waiting for limits"))
        limits <- input$limits
        
        average_type <- getAverageType()
        if (average_type == "median") {
            limits <- paste0("median.", limits)
        }
        limits
    })
    
    # Use mean or median?
    
    getAverageType <- reactive({
        validate(need(!is.null(input$average_type), "Waiting for average type"))
        input$average_type
    })
    
    # How to display clusters
    
    getClusterDisplay <- reactive({
        validate(need(!is.null(input$cluster_display), "waiting for cluster display"))
        input$cluster_display
    })
    
    ############################################################################# Display things
    
    errorBarsSubtitle <- reactive({
        limits <- getLimits()
        limit_types <- c(sd = "standard deviation", se = "standard error", ci = "95% Confidence interval")
        cluster_display <- getClusterDisplay()
        average_type = getAverageType()
        
        subtitle <- ""
        if (cluster_display != "sample_lines") {
            subtitle = paste0("(Limits show ", limit_types[limits], "s of the ", getAverageType(), ")")
        }
        subtitle
    })
    
    # Create title reflecting how the input matrix for clustering was generated
    
    output$geneClusteringTitle <- renderUI({
        list(h3(paste("Feature-wise clustering for feature set:", matrixTitle())), h4(errorBarsSubtitle()))
    })
    
    # A common set of colors however the clusters are plotted
    
    # getClusterColors <- reactive({ number_of_clusters <- getClusterNumber() makeColorScale(as.numeric(number_of_clusters), 'Dark2') })
    
    ############################################################################# Clustering
    
    # Make a scaled version of the input matrix suitable for clustering and visualisation
    
    scaleMatrix <- reactive({
        inmatrix <- selectMatrix()
        data.frame(t(scale(t(inmatrix))), check.names = FALSE)
    })
    
    # Do the actual clustering. Assign genes/rows to clusters.
    
    makeClusters <- reactive({
        scaled_inmatrix <- scaleMatrix()
        cluster_number <- getClusterNumber()
        withProgress(message = "Calculating clusters with clara()", value = 0, {
            cluster::clara(scaled_inmatrix, cluster_number, samples = 50)
        })
    })
    
    # Get the matrix of values for each cluster
    
    getMatricesByCluster <- reactive({
        scaled_inmatrix <- scaleMatrix()
        clusters <- makeClusters()
        split(scaled_inmatrix, clusters$clustering[match(rownames(scaled_inmatrix), names(clusters$clustering))])
    })
    
    # A reactive to define what summary stats are used
    
    addMedians <- reactive({
        average_type <- getAverageType()
        average_type == "median"
    })
    
    # Generate summary statistics from the matrix for each cluster. This is what's actually plotted if cluster display is set to something other than 'sample
    # lines'.  Uses summarySE() to derive mean, median, standard error, standard deviation and 95% confidence intervals.
    
    getSummarisedMatricesByCluster <- reactive({
        matrices_by_cluster <- getMatricesByCluster()
        add_medians <- addMedians()
        
        withProgress(message = "Calculating summary statistics for the clusters", value = 0, {
            lapply(matrices_by_cluster, function(mbc) {
                summarySE(reshape2::melt(as.matrix(mbc)), measurevar = "value", groupvars = "Var2", add_medians = add_medians)
            })
        })
    })
    
    # The business end of the cluster plotting. Use the above parameters and processing to produce one plot object for each cluster of matrix rows.
    
    makeClusterPlots <- reactive({
        matrices_by_cluster <- getMatricesByCluster()
        summarised_matrices_by_cluster <- getSummarisedMatricesByCluster()
        cluster_colors <- getPalette()
        limits <- getLimits()
        average_type <- getAverageType()
        cluster_display <- getClusterDisplay()
        
        if (cluster_display == "sample_lines") {
            plots <- lapply(names(matrices_by_cluster), function(c) {
                cluster_color <- cluster_colors[as.numeric(c)]
                
                x <- matrices_by_cluster[[c]]
                if (nrow(x) > 100) {
                  x <- x[sample(1:nrow(x), 100), ]
                }
                x <- reshape2::melt(as.matrix(x))
                x %>% group_by(Var1) %>% plot_ly(x = ~Var2, y = ~value, type = "scatter", mode = "lines", text = ~Var1, name = paste("Cluster", c), line = list(color = cluster_color))
            })
        } else if (cluster_display == "error_bars") {
            plots <- lapply(names(summarised_matrices_by_cluster), function(c) {
                cluster_color <- cluster_colors[as.numeric(c)]
                x <- summarised_matrices_by_cluster[[c]]
                plot_ly(x, x = ~Var2, y = ~x[, average_type], name = paste("Cluster", c)) %>% add_lines(error_y = ~list(name = paste("Cluster", c), color = cluster_color, 
                  type = "array", array = x[, limits]), line = list(color = cluster_color))
            })
        } else {
            plots <- lapply(names(matrices_by_cluster), function(c) {
                cluster_color <- cluster_colors[as.numeric(c)]
                x <- summarised_matrices_by_cluster[[c]]
                
                x$lower <- x[, average_type] - x[, limits]
                x$upper <- x[, average_type] + x[, limits]
                
                plot_ly(x, name = paste("Cluster", c)) %>% add_trace(x = ~Var2, y = x[, average_type], type = "scatter", mode = "lines", line = list(color = cluster_color), 
                  showlegend = TRUE) %>% add_trace(x = ~Var2, y = ~upper, type = "scatter", mode = "lines", line = list(color = "transparent"), showlegend = FALSE, 
                  name = paste(average_type, toupper(limits), sep = " + ")) %>% add_trace(x = ~Var2, y = ~lower, type = "scatter", mode = "lines", fill = "tonexty", 
                  fillcolor = "rgba(0,100,80,0.2)", line = list(color = "transparent"), showlegend = FALSE, name = paste(average_type, toupper(limits), sep = " - "))
            })
        }
        
        experiment <- selectColData()
        
        lapply(plots, function(p) {
            p %>% layout(xaxis = list(categoryarray = rownames(experiment), categoryorder = "array", title = ""), yaxis = list(title = "scaled<br />expression"), 
                margin = list(b = 150))
        })
    })
    
    # Take individual cluster plots and display them together using subplot.
    
    output$geneClusteringPlot <- renderPlotly({
        withProgress(message = "Plotting cluster profiles", value = 0, {
            scaled_inmatrix <- scaleMatrix()
            clusters <- makeClusters()
            
            withProgress(message = "Making a plot for each cluster", value = 0, {
                plots <- makeClusterPlots()
            })
            do.call(function(...) subplot(..., titleX = TRUE, titleY = TRUE, shareY = TRUE, shareX = TRUE, nrows = ceiling(length(plots)/3)), plots) %>% config(showLink = TRUE)
        })
    })
    
    ############################################################################# Functions for making the table of values with cluster numbers
    
    makeMatrixWithClusters <- reactive({
        inmatrix <- selectMatrix()
        clusters <- makeClusters()
        ese <- getExperiment()
        mf <- getMetafields()
        
        inmatrix <- data.frame(inmatrix, check.names = FALSE)
        
        inmatrix$cluster <- clusters$clustering[match(rownames(inmatrix), names(clusters$clustering))]
        inmatrix <- inmatrix[, c("cluster", colnames(inmatrix)[colnames(inmatrix) != "cluster"])]
        labelMatrix(inmatrix, ese, metafields = mf)
    })
    
    # Add links to the table
    
    makeLinkedMatrixWithClusters <- reactive({
        mwc <- makeMatrixWithClusters()
        linkMatrix(mwc, eselist@url_roots)
    })
    
    # Render the table and provide for download, using the simpletable module.
    
    callModule(simpletable, "geneClusteringTable", downloadMatrix = makeMatrixWithClusters, displayMatrix = makeLinkedMatrixWithClusters, filter = "top", filename = "clustered_matrix", 
        rownames = FALSE)
}

#' Summarise an input matrix
#' 
#' Matrix summarization function adapted from 
#' \url{http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)}{the R cookbook}.
#' 
#' Uses bootstrapping to return the standard error/ deviation of the median.
#' 
#' @param data A data frame. Expects all values for summarisation to be in one 
#'   column, which may requie judicious use of \code{\link[reshape2]{melt}}.
#' @param measurevar The name of a column that contains the variable to be
#'   summariezed
#' @param groupvars A vector containing names of columns that contain grouping
#'   variables
#' @param na.rm A boolean that indicates whether to ignore NA's
#' @param conf.interval The percent range of the confidence interval (default is
#'   95 percent)
#' @param add_medians Logical indicating whether medians should be added to the
#'   output. Standard error estimates for the median require bootstrapping, so 
#'   TRUE for this variables make summary statistic calculation take longer.
#'   
#' @return out Data frame with summary statistics
#' @import plyr
#' @export
#' 
#' @examples 
#' tg <- ToothGrowth
#' summarySE(tg, measurevar='len', groupvars=c('supp','dose'))

summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, add_medians = FALSE, .drop = TRUE) {
    require(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function(x, na.rm = FALSE) {
        if (na.rm) 
            sum(!is.na(x)) else length(x)
    }
    
    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop = .drop, .fun = function(xx, col, na.rm) {
        stats <- c(N = length2(xx[, col], na.rm = na.rm), mean = mean(xx[, col], na.rm = na.rm), sd = sd(xx[, col], na.rm = na.rm))
        
        if (add_medians) {
            stats["median"] = median(xx[, col], na.rm = na.rm)
            stats["median.se"] = bootstrapMedian(xx[, col], 1000)$std.err
        }
        stats
    }, measurevar, na.rm)
    
    # Rename the 'mean' column
    
    datac$se <- datac$sd/sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error Calculate t-statistic for confidence interval: e.g., if conf.interval is .95, use .975 (above/below),
    # and use df=N-1
    
    ciMult <- qt(conf.interval/2 + 0.5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    
    if (add_medians) {
        datac$median.sd <- datac$median.se * sqrt(datac$N)
        datac$median.ci <- datac$median.se * ciMult
    }
    
    return(datac)
}

#' Bootstrap the standard error of the median
#'
#' @param data Numeric vector
#' @param num Number of bootstraps
#'
#' @return out The bootstrap estimate of the median
#' @export

bootstrapMedian <- function(data, num) {
    resamples <- lapply(1:num, function(i) sample(data, replace = T))
    r.median <- sapply(resamples, median)
    std.err <- sqrt(var(r.median))
    list(std.err = std.err, resamples = resamples, medians = r.median)
} 
