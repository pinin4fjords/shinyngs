#!/usr/bin/env Rscript

library(optparse)

################################################
################################################
## Argument parsing                           ##
################################################
################################################

option_list <- list(
  make_option(
    c("-i", "--raw_count_file"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "Count file matrix where rows are genes and columns are samples."
  ),
  make_option(
    c("-j", "--normalised_count_file"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "Count file matrix where rows are genes and columns are samples."
  ),
  make_option(
    c("-k", "--variance_stabilised_count_file"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "Count file matrix where rows are genes and columns are samples."
  ),
  make_option(
    c("-s", "--sample_file"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "CSV file containing sample metadata."
  ),
  make_option(
    c("-f", "--feature_metadata"),
    type = "character",
    default = NULL,
    help = "CSV-format feature (often gene) metadata file."
  ),
  make_option(
    c("-o", "--outdir"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "Output directory."
  ),
  make_option(
    c("-v", "--contrast_variable"),
    type = "character",
    metavar = "string",
    help = "Column in sample sheet with which to form the contrast."
  ),
  make_option(
    c("-g", "--feature_id_col"),
    type = "character",
    metavar = "string",
    help = "Count file column containing gene identifiers.",
    default = "gene_id"
  ),
  make_option(
    c("-m", "--gene_name_col"),
    type = "character",
    metavar = "string",
    help = "Count file column containing gene names.",
    default = "gene_name"
  ),
  make_option(
    c("-a", "--sample_id_col"),
    type = "character",
    metavar = "string",
    help = "Sample file column name containing sample identifiers.",
    default = "sample"
  ),
  make_option(
    c("-n", "--n_genes"),
    type = "integer",
    metavar = "integer",
    help = "Number of variable genes to use for PCA and sample clustering.",
    default = 500
  ),
  make_option(
    c("-r", "--outlier_mad_threshold"),
    type = "double",
    metavar = "float",
    help = "Threshold on MAD score used to derive outlier status.",
    default = -5
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt        <- parse_args(opt_parser)

for (input in c('raw_count_file', 'sample_file', 'contrast_variable', 'outdir')){
  if (is.null(opt[[input]])) {
    print_help(opt_parser)
    stop(paste0("Please provide a ", input), call. = FALSE)
  }
}

################################################
################################################
## Library loading                            ##
################################################
################################################

library(shinyngs)

################################################
################################################
## Read input data                            ##
################################################
################################################

print("Reading inputs...")

sample.sheet <- read_metadata(
    filename = opt$sample_file,
    id_col = opt$sample_id_col,
    sep = ","
)

feature.metadata <-
  read_metadata(
    opt$feature_metadata,,
    id_col = opt$feature_id_col,
    sep = "\t",
    stringsAsFactors = FALSE
  )


# Read any provided expression matrices

expression_files <-
  list(
    raw = opt$raw_count_file,
    normalised = opt$normalised_count_file,
    variance_stabilised = opt$variance_stabilised_count_file
  )
expression_files <- Filter(Negate(is.null), expression_files)

# Expect that 'variance stabilised' expression profiles will already be logged

expression_data <- lapply(expression_files, function(x){
  read_matrix(
    x,
    sample_metadata = sample.sheet,
    feature_metadata = feature.metadata,
    sep = "\t",
    row.names = 1
  )
})
                         
                         
for (unlogged_expression_type in c('raw', 'normalised')){
  if (unlogged_expression_type %in% names(expression_data)){
    expression_data[[unlogged_expression_type]] <- log2(expression_data[[unlogged_expression_type]])
  }
}

# Create output paths

print("Creating output paths...")

png_outdir <- file.path(opt$outdir, 'png')
html_outdir <- file.path(opt$outdir, 'html') 

for (od in c(png_outdir, html_outdir)){
  dir.create(path = od, recursive = TRUE, showWarnings = FALSE)
}

# Take gene metadata from the raw matrix (if we assume the nf-core workflow there should be symbols)

gene_meta <- read_matrix(expression_files$raw, sample_sheet = sample.sheet, type = 'meta')

# Add symbols to matrix rows for display purposes

expression_data <- lapply(expression_data, function(x){
  rownames(x) <- paste(gene_meta[rownames(x),'gene_name'], '/', rownames(x))
  x
})

################################################
################################################
## Boxplots                                   ##
################################################
################################################

print("Writing boxplots...")

print("... interactive")
interactive_boxplot <- plotly_boxplot(expression_data, experiment = sample.sheet, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, 'Set1'))
htmlwidgets::saveWidget(plotly::as_widget(interactive_boxplot), file.path(html_outdir, "boxplot.html"), selfcontained = TRUE)

print("... static")
static_boxplot <- ggplot_boxplot(expression_data, experiment = sample.sheet, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, 'Set1'))
png(filename = file.path(png_outdir, "boxplot.png"), width=800, height=600)
static_boxplot
dev.off()

################################################
################################################
## Density plots                              ##
################################################
################################################

print("Writing density plots...")

print("...interactive")
interactive_densityplot <- plotly_densityplot(expression_data, experiment = sample.sheet, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, 'Set1'))
htmlwidgets::saveWidget(plotly::as_widget(interactive_densityplot), file.path(html_outdir, "density.html"), selfcontained = TRUE)

print("... static")
static_densityplot <- ggplot_densityplot(expression_data, experiment = sample.sheet, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, 'Set1'))
png(filename = file.path(png_outdir, "density.png"), width=800, height=600)
static_densityplot
dev.off()

if ('variance_stabilised' %in% names(expression_data)){

  ################################################
  ################################################
  ## PCA plots                                  ##
  ################################################
  ################################################
  
  print("Writing PCA plots...")
  pca_data <- compilePCAData(expression_data$variance_stabilised)  
  
  plotdata <- pca_data$coords
  plotdata$colorby <- factor(sample.sheet[[opt$contrast_variable]], levels = unique(sample.sheet[[opt$contrast_variable]]))
  
  # Make plotting data combining PCA coords with coloring groups etc
  
  plotdata$name <- rownames(plotdata)
  percentVar <- pca_data$percentVar
  labels <- paste0(colnames(plotdata), ' (', sprintf("%.1f", (percentVar * 100)), '%)')
  ncats <- length(unique(plotdata$colorby))
  
  plot_types <- list('2' = 'scatter', '3' ='scatter3d')
  
  for (d in names(plot_types)){
    
    # Default plot args whatever we're doing
    
    plot_args <- list(
      x = pca_data$coords[,1],
      y = pca_data$coords[,2],
      xlab = labels[1],
      ylab = labels[2],
      colorby = plotdata$colorby,
      plot_type = plot_types[[d]],
      palette = makeColorScale(length(unique(plotdata$colorby)), 'Set1'),
      legend_title = prettifyVariablename(opt$contrast_variable),
      labels = plotdata$name,
      show_labels = TRUE
    )
    
    print(paste0("...static (",d,"d)"))

    if (d == '3'){
      plot_args$z <- pca_data$coords[,3]
      plot_args$zlab <- labels[3]
    }
    
    p <- do.call('static_scatterplot', plot_args)
    png(filename = file.path(png_outdir, paste0("pca", d,"d.png")), width=800, height=600)
    print(p)
    dev.off()
  
    print(paste0("...interactive (",d,"d)"))
    interactive_pcaplot <- do.call('plotly_scatterplot', plot_args)
    htmlwidgets::saveWidget(plotly::as_widget(interactive_pcaplot), file.path(html_outdir, paste0("pca", d, "d.html")), selfcontained = TRUE)
  }
  
  ################################################
  ################################################
  ## Sample dendrogram                          ##
  ################################################
  ################################################
  
  print("Writing sample dendrogram...")
  png(
    filename = file.path(png_outdir, paste0("sample_dendrogram.png")),
    width = 800,
    height = 600
  )
  clusteringDendrogram(
    2^expression_data$variance_stabilised[selectVariableGenes(matrix = expression_data$variance_stabilised, ntop = opt$n_genes), ],
    sample.sheet[, opt$contrast_variable, drop = FALSE],
    colorby = opt$contrast_variable,
    cor_method = 'spearman',
    plot_title = paste0("Sample clustering dendrogram, ", opt$n_genes, " most variable genes"),
    cluster_method = "ward.D2",
    palette = makeColorScale(length(unique(sample.sheet[[opt$contrast_variable]])), palette = "Set1")
  )
  dev.off()
  
  ################################################
  ################################################
  ## MAD score plots for outlier prediction     ##
  ################################################
  ################################################
  
  plotdata <-
    madScore(
      matrix = expression_data$variance_stabilised,
      sample_sheet = sample.sheet,
      groupby = opt$contrast_variable
    )
  
  mad_plot_args <- list(
    x = plotdata$group,
    y = plotdata$mad,
    color = plotdata$outlier,
    hline_thresholds = c("Outlier threshold" = -5),
    palette = makeColorScale(2, palette = "Set1"),
    legend_title="Outlier status",
    labels = rownames(plotdata),
    show_labels = TRUE,
    xlab = "Sample group",
    ylab = "MAD score"
  )
  
  print("MAD correlation plots...")
  print("...interactive")
  interactive_madplot <- do.call(plotly_scatterplot, mad_plot_args)
  htmlwidgets::saveWidget(plotly::as_widget(interactive_madplot), file.path(html_outdir, "mad_correlation.html"), selfcontained = TRUE)
  
  print("... static")
  static_madplot <- do.call(static_scatterplot, mad_plot_args)
  png(filename = file.path(png_outdir, "mad_correlation.png"), width=800, height=600)
  print(static_madplot)
  dev.off()
  
}
