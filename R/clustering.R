

clusteringInput <- function(id, eselist) {
    
    ns <- NS(id)
    
    expression_filters <- selectmatrixInput(ns("clustering"), eselist)
    
    cluster_displays <- c('Sample lines' = 'sample_lines', 'Error bars' = 'error_bars', 'Filled line' = 'filled_line') 
    cluster_filters <- list(
      selectInput(ns('cluster_display'), label = 'Cluster display', choices = cluster_displays, selected = 'filled_line'),
      selectInput(ns('cluster_number'), label = 'Number of clusters', choices = 1:12, selected = 6) 
    )
    
    
    fieldSets(ns("fieldset"), list(clustering = cluster_filters, expression = expression_filters, export = simpletableInput(ns("geneClusteringTable"))))
}



clusteringOutput <- function(id) {
    ns <- NS(id)
    list(modalInput(ns("clustering"), "help", "help"), modalOutput(ns("clustering"), "Feature-wise clustering", includeMarkdown(system.file("inlinehelp", "clustering.md", 
        package = packageName()))), h3("Feature-wise clustering"), plotlyOutput(ns("geneClusteringPlot"), height = 600), simpletableOutput(ns("geneClusteringTable")))
}



clustering <- function(input, output, session, eselist) {
    
    # Get the expression matrix - no need for a gene selection
    
    unpack.list(callModule(selectmatrix, "clustering", eselist, select_genes = TRUE, var_n = 1000, provide_all_genes = TRUE, default_gene_select = "variance"))
    colorBy <- callModule(groupby, "clustering", eselist = eselist, group_label = "Color by")
    
    # Make a scaled version of the input matrix suitable for clustering and 
    # visualisation
    
    scaleMatrix <- reactive({
        inmatrix <- selectMatrix()
        data.frame(t(scale(t(inmatrix))))
    })
    
    getClusterNumber <- reactive({
        validate(need(! is.null(input$cluster_number), 'waiting for cluster number'))
        input$cluster_number
    })
    
    # Assign genes/rows to clusters
    
    makeClusters <- reactive({
      scaled_inmatrix <- scaleMatrix()
      cluster_number <- getClusterNumber()
      cluster::clara(scaled_inmatrix, cluster_number, samples = 50)
    })
    
    makeMatrixWithClusters <- reactive({
      inmatrix <- selectMatrix()
      clusters <- makeClusters()
      ese <- getExperiment()
      mf <- getMetafields()
      
      inmatrix <- data.frame(inmatrix)
      
      inmatrix$cluster <- clusters$clustering[match(rownames(inmatrix), names(clusters$clustering))]
      inmatrix <- inmatrix[,c('cluster', colnames(inmatrix)[colnames(inmatrix) != 'cluster'])]
      labelMatrix(inmatrix, ese, metafields = mf)
    })
    
    makeLinkedMatrixWithClusters <- reactive({
        mwc <- makeMatrixWithClusters()
        linkMatrix(mwc, eselist@url_roots)
    })
    
    getClusterDisplay <- reactive({
        validate(need(! is.null(input$cluster_display), 'waiting for cluster display'))
        input$cluster_display
    })
    
    getMatricesByCluster <- reactive({
      scaled_inmatrix <- scaleMatrix()
      clusters <- makeClusters()
      split(scaled_inmatrix, clusters$clustering[match(rownames(scaled_inmatrix), names(clusters$clustering))])
    })
    
    getSummarisedMatricesByCluster <- reactive({
      matrices_by_cluster <- getMatricesByCluster()
      
      lapply(matrices_by_cluster, function(mbc){
        summarySE(reshape2::melt(as.matrix(mbc)), measurevar = 'value', groupvars = 'Var2')
      })
    })
    
    getClusterColors <- reactive({
        number_of_clusters <- getClusterNumber()
        makeColorScale(as.numeric(number_of_clusters), 'Dark2')
    })
    
    makeClusterPlots <- reactive({
      matrices_by_cluster <- getMatricesByCluster()
      summarised_matrices_by_cluster <- getSummarisedMatricesByCluster()
      cluster_colors <- getClusterColors()
      
      
      cluster_display <- getClusterDisplay()
      
      if (cluster_display == 'sample_lines'){
        plots <- lapply(names(matrices_by_cluster), function(c){
          cluster_color <- cluster_colors[as.numeric(c)]
          
          x <- matrices_by_cluster[[c]]
          if (nrow(x) > 100){
            x <- x[sample(1:nrow(x), 100),] 
          }
          x <- reshape2::melt(as.matrix(x))
          x %>% group_by(Var1) %>% plot_ly(x = ~Var2, y = ~value, type = 'scatter', mode = 'lines', text = ~Var1, name = paste('Cluster', c), line = list(color=cluster_color))
        })  
      }else if (cluster_display == 'error_bars'){
        plots <- lapply(names(summarised_matrices_by_cluster), function(c){
          cluster_color <- cluster_colors[as.numeric(c)]
          x <- summarised_matrices_by_cluster[[c]]
          plot_ly(x, x = ~Var2, y = ~median, name = paste('Cluster', c), color=cluster_color) %>% add_lines(error_y = ~list(value = se), line = list(color=cluster_color))
        })
      }else{
        plots <- lapply(names(matrices_by_cluster), function(c){
          cluster_color <- cluster_colors[as.numeric(c)]
          x <- summarised_matrices_by_cluster[[c]]
          type <- 'sd'
          x$lower <- x$median - x$sd
          x$upper <- x$median + x$sd
          
          plot_ly(x, x = ~Var2, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = 'transparent'),
                  showlegend = FALSE, name = paste('Median + SE')) %>%
            add_trace(y = ~lower, type = 'scatter', mode = 'lines',
                      fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                      showlegend = FALSE, name = 'Median - SE') %>%
            add_trace(x = ~Var2, y = ~median, type = 'scatter', mode = 'lines',
                      line = list(color=cluster_color),
                      name = paste('Cluster', c), showlegend = TRUE)
        })
      }
      
      experiment <- selectColData()
      
      lapply(plots, function(p){
        p %>% layout(xaxis = list(categoryarray = rownames(experiment), categoryorder = "array", title = ''), yaxis = list(title = 'scaled expression'), margin = list(b = 150))
      })
    })
    
    
    
    
    # Render the actual plot
    
    output$geneClusteringPlot <- renderPlotly({
        withProgress(message = "Making cluster plots dendrogram", value = 0, {
          scaled_inmatrix <- scaleMatrix()  
          clusters <- makeClusters()
          
            
          plots <- makeClusterPlots()
          do.call(function(...) subplot(..., titleX = TRUE, titleY = TRUE, shareY = TRUE, shareX = TRUE, nrows = ceiling(length(plots)/3)), plots)


            
        })
    })
    
    # Render the table
    
    callModule(simpletable, "geneClusteringTable", downloadMatrix = makeMatrixWithClusters, displayMatrix = makeLinkedMatrixWithClusters, filename = "clustered_matrix", 
               rownames = FALSE)
}


## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This is does the summary; it's not easy to understand...
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      median = median (xx[,col], na.rm=na.rm),
                      median.se = b.median(xx[,col], 1000)$std.err,
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  # Rename the "mean" column    
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#function which will bootstrap the standard error of the median
b.median <- function(data, num) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)   
}
