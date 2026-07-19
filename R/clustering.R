clustering_modal <- list(id = "clustering", title = "Feature-wise clustering")

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
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#' clusteringInput("myid", eselist)
#'
clusteringInput <- function(id, eselist) {
  ns <- NS(id)

  expression_filters <- selectmatrixInput(ns("clustering"), eselist)

  cluster_displays <- c(`Sample lines` = "sample_lines", `Error bars` = "error_bars", `Filled line` = "filled_line")

  cluster_filters <- list(selectInput(ns("cluster_number"), label = "Number of clusters", choices = 1:12, selected = 6), selectInput(ns("cluster_display"),
    label = "Cluster display", choices = cluster_displays, selected = "filled_line"
  ), colormakerInput(ns("clustering")), selectInput(ns("average_type"),
    label = "Average type", choices = c("mean", "median"), selected = "mean"
  ), selectInput(ns("limits"), label = "Limits show", choices = c(
    `Standard deviation` = "sd",
    `Standard error` = "se", `95% confidence interval` = "ci"
  ), selected = "sd"))

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
#' clusteringOutput("myid")
#'
clusteringOutput <- function(id) {
  ns <- NS(id)
  moduleMain(
    NULL,
    uiOutput(ns("geneClusteringTitle")),
    shinycssloaders::withSpinner(plotlyOutput(ns("geneClusteringPlot"), height = 600), color = shinyngsSpinnerColor()),
    h4("Table of values by cluster"),
    simpletableOutput(ns("geneClusteringTable"), spinner = TRUE),
    help = modalInput(ns(clustering_modal$id), "help", "help")
  )
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
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example).
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' clustering("myid", eselist)
#'
clustering <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    modalServer(clustering_modal$id, clustering_modal$title)

    # Call the selectmatrix module to get the expression matrix - no need for a gene selection

    selectmatrix_reactives <- selectmatrix("clustering", eselist, select_genes = TRUE, var_n = 1000, provide_all_genes = TRUE, default_gene_select = "variance")

    getPalette <- colormaker("clustering", getNumberCategories = getClusterNumber)

    ############################################################################# Form accessors

    # The number of clusters

    getClusterNumber <- reactive({
      validate(need(!is.null(input$cluster_number), "waiting for cluster number"))
      as.numeric(input$cluster_number)
    })

    # How limits of error bars etc should be defined ('sd', 'se' or 'ci' -
    # plotly_cluster_profiles() derives the mean/median-specific column name)

    getLimits <- reactive({
      validate(need(!is.null(input$limits), "Waiting for limits"))
      input$limits
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
      average_type <- getAverageType()

      subtitle <- ""
      if (cluster_display != "sample_lines") {
        subtitle <- paste0("(Limits show ", limit_types[limits], "s of the ", getAverageType(), ")")
      }
      subtitle
    })

    # Create title reflecting how the input matrix for clustering was generated

    output$geneClusteringTitle <- renderUI({
      list(h3(paste("Feature-wise clustering for feature set:", selectmatrix_reactives$matrixTitle())), h4(errorBarsSubtitle()))
    })

    # A common set of colors however the clusters are plotted

    # getClusterColors <- reactive({ number_of_clusters <- getClusterNumber() makeColorScale(as.numeric(number_of_clusters), 'Dark2') })

    ############################################################################# Clustering

    # Make a scaled version of the input matrix suitable for clustering and visualisation

    scaleMatrix <- reactive({
      inmatrix <- selectmatrix_reactives$selectMatrix()
      data.frame(t(scale(t(inmatrix))), check.names = FALSE)
    })

    # Do the actual clustering. Assign genes/rows to clusters. Cached on the
    # scaled matrix and cluster count so display-only controls (colors, error
    # bar type, average type) don't re-run clara().

    makeClusters <- reactive({
      scaled_inmatrix <- scaleMatrix()
      cluster_number <- getClusterNumber()
      withProgress(message = "Calculating clusters with clara()", value = 0, {
        validateOrCatch(runClustering(scaled_inmatrix, cluster_number))
      })
    }) %>% bindCache(scaleMatrix(), getClusterNumber())

    # Get the matrix of values for each cluster

    getMatricesByCluster <- reactive({
      scaled_inmatrix <- scaleMatrix()
      clusters <- makeClusters()
      split(scaled_inmatrix, clusters$clustering[match(rownames(scaled_inmatrix), names(clusters$clustering))])
    })

    # Take individual cluster plots and display them together using subplot.

    getClusterPlot <- reactive({
      matrices_by_cluster <- getMatricesByCluster()
      experiment <- selectmatrix_reactives$selectColData()

      withProgress(message = "Plotting cluster profiles", value = 0, {
        plotly_cluster_profiles(
          matrices_by_cluster,
          cluster_display = getClusterDisplay(), average_type = getAverageType(), limits = getLimits(),
          colors = getPalette(), sample_order = rownames(experiment)
        )
      })
    }) %>% bindCache(
      getMatricesByCluster(), getClusterDisplay(), getAverageType(), getLimits(), getPalette(), selectmatrix_reactives$selectColData()
    )

    output$geneClusteringPlot <- renderPlotly({
      getClusterPlot() %>% shinyngsPlotlyConfig("clustering", format = session$userData$plotFormat())
    })

    ############################################################################# Functions for making the table of values with cluster numbers

    makeMatrixWithClusters <- reactive({
      inmatrix <- selectmatrix_reactives$selectMatrix()
      clusters <- makeClusters()
      ese <- selectmatrix_reactives$getExperiment()
      mf <- selectmatrix_reactives$getMetafields()

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

    simpletable("geneClusteringTable", downloadMatrix = makeMatrixWithClusters, displayMatrix = makeLinkedMatrixWithClusters, filter = "top", filename = "clustered_matrix", rownames = FALSE)
  })
}

#' Partition the rows of a matrix into clusters with clara()
#'
#' Common function for the clustering module, validating the requested
#' cluster count against the number of rows available before calling
#' \code{\link[cluster]{clara}}.
#'
#' @param matrix Matrix with the rows to be clustered (features by row)
#' @param k Number of clusters requested
#'
#' @return output Output of \code{cluster::clara}
#'
#' @keywords shiny
#'
#' @export
#'
#' @examples
#' runClustering(matrix(rnorm(40), nrow = 10), 3)
#'
runClustering <- function(matrix, k) {
  k <- as.numeric(k)

  if (nrow(matrix) < 2) {
    stop("Clustering requires at least 2 features; only ", nrow(matrix), " were supplied.")
  }

  if (k < 1 || k > nrow(matrix)) {
    stop("Number of clusters (", k, ") must be between 1 and the number of available features (", nrow(matrix), ").")
  }

  cluster::clara(matrix, k, samples = 50)
}

#' Summarise an input matrix
#'
#' Matrix summarization function adapted from
#' \url{http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)}{the R cookbook}.
#'
#' Uses bootstrapping to return the standard error/ deviation of the median.
#'
#' @param data A data frame. Expects all values for summarisation to be in one
#'   column, which may require judicious use of \code{melt_matrix}.
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
#' @param .drop Logical controlling whether unobserved combinations of
#'   \code{groupvars} are dropped, passed to \code{dplyr::group_by()}.
#'
#' @return out Data frame with summary statistics
#'
#' @export
#'
#' @examples
#' tg <- ToothGrowth
#' summarySE(tg, measurevar = "len", groupvars = c("supp", "dose"))
#'
summarySE <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = 0.95, add_medians = FALSE, .drop = TRUE) {
  # New version of length which can handle NA's: if na.rm==TRUE, don't count them
  length2 <- function(x, na.rm = FALSE) {
    if (na.rm) {
      sum(!is.na(x))
    } else {
      length(x)
    }
  }

  datac <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groupvars)), .drop = .drop) %>%
    dplyr::summarise(
      N = length2(.data[[measurevar]], na.rm = na.rm),
      mean = mean(.data[[measurevar]], na.rm = na.rm),
      sd = sd(.data[[measurevar]], na.rm = na.rm),
      median = if (add_medians) median(.data[[measurevar]], na.rm = na.rm) else NULL,
      median.se = if (add_medians) bootstrapMedian(.data[[measurevar]], 1000)$std.err else NULL,
      .groups = "drop"
    ) %>%
    as.data.frame()

  datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

  # Confidence interval multiplier for standard error Calculate t-statistic for confidence interval: e.g., if conf.interval is .95, use .975 (above/below),
  # and use df=N-1

  ciMult <- qt(conf.interval / 2 + 0.5, datac$N - 1)
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
  resamples <- lapply(seq_len(num), function(i) sample(data, replace = TRUE))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err = std.err, resamples = resamples, medians = r.median)
}

#' Calculate MAD scores as per OmicSoft
#'
#' Folllows description at
#' https://wiki.arrayserver.com/wiki/index.php?title=CorrelationQC.pdf.
#'
#' Not currently deployed anywhere in shinyngs, but a potential way of flagging
#' outliers for investigation
#'
#' @param matrix Matrix with samples by column
#' @param sample_sheet Sample sheet with samples by row
#' @param groupby Sample sheet column that can be used to group samples
#' @param outlier_threshold Value below which points should be flagged as
#'   outliers, conventionally -5
#'
#' @export
#'
#' @return mad_score A data frame with columns for group name, MAD score and outlier status. A
#'   threshold of < -5 usually indicates outliers

madScore <- function(matrix, sample_sheet = NULL, groupby = NULL, outlier_threshold = -5) {
  # Double-check the matrix/ sample sheet synch
  matrix <- matrix[, rownames(sample_sheet), drop = FALSE]

  group_sizes <- table(sample_sheet[[groupby]])
  mad_valid_groups <- group_sizes[group_sizes > 2]

  if (length(mad_valid_groups) == 0) {
    print("WARNING: low replication, skipping outlier detection ...")
    NULL
  } else {
    mad_valid_samples <- which(sample_sheet[[groupby]] %in% names(mad_valid_groups))

    matrix <- matrix[, mad_valid_samples, drop = FALSE]
    sample_sheet <- sample_sheet[mad_valid_samples, , drop = FALSE]

    if (is.null(groupby)) {
      matrices <- list(all = matrix)
    } else {
      matrices <- lapply(split(rownames(sample_sheet), sample_sheet[[groupby]]), function(x) matrix[, x, drop = FALSE])
    }

    # The following intentionally verbose to highlight logic

    mads <- do.call(rbind, lapply(names(matrices), function(g) {
      corrs <- cor(matrices[[g]])
      corrs_involving <- unlist(lapply(seq_len(ncol(corrs)), function(x) mean(corrs[x, -x])))
      corrs_not_involving <- unlist(lapply(seq_len(ncol(corrs)), function(x) mean(corrs[-x, -x][upper.tri(corrs[-x, -x])])))

      correlation_difference <- structure(corrs_involving - corrs_not_involving, names = colnames(corrs))
      median_correlation_differences <- median(correlation_difference)
      median_absolute_deviation <- median(abs(correlation_difference - median_correlation_differences))
      data.frame(group = g, mad = (correlation_difference - median_correlation_differences) / (median_absolute_deviation * 1.4826))
    }))

    mads$outlier <- mads$mad < outlier_threshold
    mads
  }
}

#' Plot expression profiles for a set of feature clusters with \code{plot_ly()}
#'
#' Draws one sub-plot per cluster of a scaled expression matrix (e.g. as
#' produced by \code{\link{runClustering}}), combined with
#' \code{plotly::subplot()}. Three display modes are available: individual
#' sample lines, a mean/median line with error bars, or a mean/median line
#' with a shaded error band. Summary statistics (mean, median and their
#' spreads) are derived internally with \code{\link{summarySE}}.
#'
#' @param matrices_by_cluster A named list of matrices/data frames, one per
#'   cluster, features by row and samples by column (e.g. the input matrix
#'   split by \code{clustering$clustering} from \code{\link{runClustering}}).
#'   Names should be the cluster numbers as produced by \code{split()}, since
#'   \code{colors} is indexed by cluster number.
#' @param cluster_display 'filled_line' (a mean/median line with a shaded
#'   error band), 'sample_lines' (individual sample profiles) or
#'   'error_bars' (a mean/median line with error bars)
#' @param average_type 'mean' or 'median'
#' @param limits Which spread to show around the average: 'sd' (standard
#'   deviation), 'se' (standard error) or 'ci' (95% confidence interval).
#'   Ignored when \code{cluster_display = "sample_lines"}
#' @param colors A vector of colors, indexed by cluster number (i.e.
#'   \code{colors[[2]]} colors the cluster named "2" in
#'   \code{matrices_by_cluster}). Defaults to \code{\link{makeColorScale}}
#' @param sample_order Character vector giving the sample (x axis) display
#'   order. Defaults to \code{colnames()} of the first cluster's matrix
#' @param max_sample_lines Maximum number of sample lines drawn per cluster
#'   under \code{cluster_display = "sample_lines"}; larger clusters are
#'   randomly subsampled to this many rows
#'
#' @return output A plotly htmlwidget
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' matrices_by_cluster <- list(
#'   `1` = matrix(rnorm(30), nrow = 5, dimnames = list(paste0("gene", 1:5), paste0("sample", 1:6))),
#'   `2` = matrix(rnorm(30), nrow = 5, dimnames = list(paste0("gene", 6:10), paste0("sample", 1:6)))
#' )
#'
#' plotly_cluster_profiles(matrices_by_cluster, cluster_display = "filled_line")
#'
plotly_cluster_profiles <- function(matrices_by_cluster, cluster_display = c("filled_line", "sample_lines", "error_bars"),
                                     average_type = c("mean", "median"), limits = c("sd", "se", "ci"), colors = NULL,
                                     sample_order = NULL, max_sample_lines = 100) {
  cluster_display <- match.arg(cluster_display)
  average_type <- match.arg(average_type)
  limits <- match.arg(limits)

  if (average_type == "median") {
    limits <- paste0("median.", limits)
  }
  if (is.null(colors)) {
    colors <- makeColorScale(length(matrices_by_cluster))
  }
  if (is.null(sample_order)) {
    sample_order <- colnames(matrices_by_cluster[[1]])
  }

  summarised_matrices_by_cluster <- lapply(matrices_by_cluster, function(mbc) {
    summarySE(melt_matrix(as.matrix(mbc)), measurevar = "value", groupvars = "Var2", add_medians = average_type == "median")
  })

  if (cluster_display == "sample_lines") {
    plots <- lapply(names(matrices_by_cluster), function(c) {
      cluster_color <- colors[as.numeric(c)]

      x <- matrices_by_cluster[[c]]
      if (nrow(x) > max_sample_lines) {
        x <- x[sample(seq_len(nrow(x)), max_sample_lines), ]
      }
      x <- melt_matrix(as.matrix(x))
      x %>%
        dplyr::group_by(Var1) %>%
        plot_ly(x = ~Var2, y = ~value, type = "scatter", mode = "lines", text = ~Var1, name = paste("Cluster", c), line = list(color = cluster_color))
    })
  } else if (cluster_display == "error_bars") {
    plots <- lapply(names(summarised_matrices_by_cluster), function(c) {
      cluster_color <- colors[as.numeric(c)]
      x <- summarised_matrices_by_cluster[[c]]
      plot_ly(x, x = ~Var2, y = ~ x[, average_type], name = paste("Cluster", c)) %>% add_lines(error_y = ~ list(
        name = paste("Cluster", c), color = cluster_color,
        type = "array", array = x[, limits]
      ), line = list(color = cluster_color))
    })
  } else {
    plots <- lapply(names(matrices_by_cluster), function(c) {
      cluster_color <- colors[as.numeric(c)]
      x <- summarised_matrices_by_cluster[[c]]

      x$lower <- x[, average_type] - x[, limits]
      x$upper <- x[, average_type] + x[, limits]

      plot_ly(x, name = paste("Cluster", c)) %>%
        add_trace(
          x = ~Var2, y = x[, average_type], type = "scatter", mode = "lines", line = list(color = cluster_color),
          showlegend = TRUE
        ) %>%
        add_trace(
          x = ~Var2, y = ~upper, type = "scatter", mode = "lines", line = list(color = "transparent"), showlegend = FALSE,
          name = paste(average_type, toupper(limits), sep = " + ")
        ) %>%
        add_trace(
          x = ~Var2, y = ~lower, type = "scatter", mode = "lines", fill = "tonexty",
          fillcolor = "rgba(0,100,80,0.2)", line = list(color = "transparent"), showlegend = FALSE, name = paste(average_type, toupper(limits), sep = " - ")
        )
    })
  }

  plots <- lapply(plots, function(p) {
    p %>% layout(
      xaxis = list(categoryarray = sample_order, categoryorder = "array", title = ""), yaxis = list(title = "scaled<br />expression"),
      margin = list(b = 150)
    )
  })

  do.call(function(...) subplot(..., titleX = TRUE, titleY = TRUE, shareY = TRUE, shareX = TRUE, nrows = ceiling(length(plots) / 3)), plots)
}
