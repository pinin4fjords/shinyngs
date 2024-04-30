#!/usr/bin/env Rscript

library(optparse)

################################################
################################################
## Argument parsing                           ##
################################################
################################################

option_list <- list(
  make_option(
    c("-e", "--assay_files"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of TSV-format file expression matrix files."
  ),
  make_option(
    c("-w", "--assay_names"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of names of same length as --assay-files."
  ),
  make_option(
    c("-i", "--final_assay"),
    type = "character",
    default = NULL,
    help = "String or integer indicating final assay to be used in plot assuming minimal normalisation etc has occured- e.g. PCA. Default is last element specified in --assay_files"
  ),
  make_option(
    c("-s", "--sample_metadata"),
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
    c("-m", "--feature_name_col"),
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
  ),
  make_option(
    c("-x", "--write_html"),
    action = "store_true",
    default = FALSE,
    help = "Set this option to produce HTML outputs as well as PNGs."
  ),
  make_option(
    c("-p", "--palette_name"),
    type = "character",
    metavar = "string",
    help = "A valid R palette name.",
    default = "Set1"
  ),
  make_option(
    c("-l", "--log2_assays"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of assay_names to which to apply log2. Alternatively, comma-separated list of positive integers indicating which assays to log (1-based!). If not specified, the script will guess the log status based on the maximum value of the input data. If empty string, will not apply log2."
  ),
  make_option(
    c("-k", "--log2_guessing_threshold"),
    type = "integer",
    metavar = "integer",
    help = "Magnitude used to guess log status.",
    default = 30
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

for (input in c("assay_files", "sample_metadata", "feature_metadata", "contrast_variable", "outdir")) {
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

sample_metadata <- read_metadata(
  filename = opt$sample_metadata,
  id_col = opt$sample_id_col
)

feature_metadata <-
  read_metadata(
    opt$feature_metadata,
    id_col = opt$feature_id_col,
    stringsAsFactors = FALSE
  )

# Read any provided expression matrices

assay_files <-
  stringsToNamedVector(
    elements_string = opt$assay_files,
    opt$assay_names,
    simplify_files = TRUE,
    prettify_names = FALSE
  )

# Valid samples are those with values in the specified sample metadata field

valid_samples <- Filter(function(x) !is.na(x) && x != '' && !is.null(x), sample_metadata[[opt$contrast_variable]])
sample_metadata <- sample_metadata[sample_metadata[[opt$contrast_variable]] %in% valid_samples, ]

# Expect that 'variance stabilised' expression profiles will already be logged

assay_data <- lapply(assay_files, function(x) {
  na.omit(
    read_matrix(
      x,
      sample_metadata = sample_metadata,
      feature_metadata = feature_metadata,
      row.names = 1
    )
  )
})

# Check an indicated final assay is among what we have

if (is.null(opt$final_assay)){
  final_assay <- length(assay_data)
}else{
  final_assay <- opt$final_assay
  if (!final_assay %in% names(assay_data)) {
    if (!grepl("\\D", final_assay)) {
      final_assay <- names(assay_data)[as.numeric(final_assay)]
    } else {
      stop(paste0("Indicated final assay '", final_assay, "' not among ", paste(names(assay_data), collapse = ",")))
    }
  }
}

# Apply a log2 transform to assays where appropriate
assay_data <- cond_log2_transform_assays(assay_data, log2_assays = opt$log2_assays, threshold = opt$log2_guessing_threshold)

# Prettify assay names
names(assay_data) <- prettifyVariablename(names(assay_data))
final_assay <- prettifyVariablename(names(assay_data))
                        
# Create output paths

print("Creating output paths...")

png_outdir <- file.path(opt$outdir, 'png')
html_outdir <- file.path(opt$outdir, 'html') 

dir.create(path = png_outdir, recursive = TRUE, showWarnings = FALSE)

if (opt$write_html){
    dir.create(path = html_outdir, recursive = TRUE, showWarnings = FALSE)
}

# Add symbols to matrix rows for display purposes

assay_data <- lapply(assay_data, function(x) {
  gene_symbols <- feature_metadata[match(rownames(x), feature_metadata[[opt$feature_id_col]]), opt$feature_name_col]
  rownames(x) <- paste(gene_symbols, "/", rownames(x))
  x
})

################################################
################################################
## Boxplots                                   ##
################################################
################################################

print("Writing boxplots...")

if (opt$write_html){
  print("... interactive")
  interactive_boxplot <- plotly_boxplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette_name = opt$palette_name, should_log = FALSE)
  htmlwidgets::saveWidget(plotly::as_widget(interactive_boxplot), file.path(html_outdir, "boxplot.html"), selfcontained = TRUE)
}

print("... static")
static_boxplot <- ggplot_boxplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette_name = opt$palette_name, should_log = FALSE)
png(filename = file.path(png_outdir, "boxplot.png"), width = 800, height = 600)
static_boxplot
dev.off()

################################################
################################################
## Density plots                              ##
################################################
################################################

print("Writing density plots...")

if (opt$write_html){
  print("...interactive")
  interactive_densityplot <- plotly_densityplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette_name = opt$palette_name, should_log = FALSE)
  htmlwidgets::saveWidget(plotly::as_widget(interactive_densityplot), file.path(html_outdir, "density.html"), selfcontained = TRUE)
}

print("... static")
static_densityplot <- ggplot_densityplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette_name = opt$palette_name, should_log = FALSE)
png(filename = file.path(png_outdir, "density.png"), width = 800, height = 600)
static_densityplot
dev.off()

################################################
################################################
## PCA plots                                  ##
################################################
################################################

print("Writing PCA plots...")
pca_data <- compilePCAData(assay_data[[final_assay]])

plotdata <- pca_data$coords
plotdata$colorby <- factor(sample_metadata[[opt$contrast_variable]], levels = unique(sample_metadata[[opt$contrast_variable]]))

# Make plotting data combining PCA coords with coloring groups etc

plotdata$name <- rownames(plotdata)
percentVar <- pca_data$percentVar
labels <- paste0(colnames(plotdata), " (", sprintf("%.1f", percentVar), "%)")
ncats <- length(unique(plotdata$colorby))

plot_types <- list("2" = "scatter", "3" = "scatter3d")

for (d in names(plot_types)) {

  # Default plot args whatever we're doing

  plot_args <- list(
    x = pca_data$coords[, 1],
    y = pca_data$coords[, 2],
    xlab = labels[1],
    ylab = labels[2],
    colorby = plotdata$colorby,
    plot_type = plot_types[[d]],
    legend_title = prettifyVariablename(opt$contrast_variable),
    labels = plotdata$name,
    show_labels = TRUE,
    palette_name = opt$palette_name
  )

  print(paste0("...static (", d, "d)"))

  if (d == "3") {
    plot_args$z <- pca_data$coords[, 3]
    plot_args$zlab <- labels[3]
  }

  png(filename = file.path(png_outdir, paste0("pca", d, "d.png")), width = 800, height = 600)
  p <- do.call("static_scatterplot", plot_args)
  print(p)
  dev.off()

  if (opt$write_html){
    print(paste0("...interactive (", d, "d)"))
    interactive_pcaplot <- do.call("plotly_scatterplot", plot_args)
    htmlwidgets::saveWidget(plotly::as_widget(interactive_pcaplot), file.path(html_outdir, paste0("pca", d, "d.html")), selfcontained = TRUE)
  }
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
  2^assay_data[[final_assay]][selectVariableGenes(matrix = assay_data[[final_assay]], ntop = opt$n_genes), ],
  sample_metadata[, opt$contrast_variable, drop = FALSE],
  colorby = opt$contrast_variable,
  cor_method = "spearman",
  plot_title = paste0("Sample clustering dendrogram, ", opt$n_genes, " most variable genes"),
  cluster_method = "ward.D2",
  palette_name = opt$palette_name
)
dev.off()

################################################
################################################
## MAD score plots for outlier prediction     ##
################################################
################################################

plotdata <-
  madScore(
    matrix = assay_data[[final_assay]],
    sample_sheet = sample_metadata,
    groupby = opt$contrast_variable
  )

if (! is.null(plotdata)){
  mad_plot_args <- list(
    x = plotdata$group,
    y = plotdata$mad,
    color = plotdata$outlier,
    hline_thresholds = c("Outlier threshold" = -5),
    legend_title = "Outlier status",
    labels = rownames(plotdata),
    show_labels = TRUE,
    xlab = "Sample group",
    ylab = "MAD score",
    palette_name = opt$palette_name
  )

  print("MAD correlation plots...")
  if (opt$write_html){
    print("...interactive")
    interactive_madplot <- do.call(plotly_scatterplot, mad_plot_args)
    htmlwidgets::saveWidget(plotly::as_widget(interactive_madplot), file.path(html_outdir, "mad_correlation.html"), selfcontained = TRUE)
  }

  print("... static")
  static_madplot <- do.call(static_scatterplot, mad_plot_args)
  png(filename = file.path(png_outdir, "mad_correlation.png"), width = 800, height = 600)
  print(static_madplot)
  dev.off()
}
