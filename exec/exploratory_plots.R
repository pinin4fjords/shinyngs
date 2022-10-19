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
    c("-i", "--final-assay"),
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
  id_col = opt$sample_id_col,
  sep = ","
)

feature_metadata <-
  read_metadata(
    opt$feature_metadata,
    id_col = opt$feature_id_col,
    sep = "\t",
    stringsAsFactors = FALSE
  )

# Read any provided expression matrices

assay_files <-
  stringsToNamedVector(
    elements_string = opt$assay_files,
    opt$assay_names,
    simplify_files = TRUE,
    prettify_names = TRUE
  )

# Expect that 'variance stabilised' expression profiles will already be logged

assay_data <- lapply(assay_files, function(x) {
  read_matrix(
    x,
    sample_metadata = sample_metadata,
    feature_metadata = feature_metadata,
    sep = "\t",
    row.names = 1
  )
})

# Check an indicated final assay is among what we have

if (is.null(opt$final_assay)){
  final_assay <- length(assay_data)
}else{
  final_assay <- prettifyVariablename(opt$final_assay)
  if (!final_assay %in% names(assay_data)) {
    if (!grepl("\\D", final_assay)) {
      final_assay <- names(assay_data)[as.numeric(final_assay)]
    } else {
      stop(paste0("Indicated final assay '", final_assay, "' not among ", paste(names(assay_data), collapse = ",")))
    }
  }
}

# Guess which assays aren't logged and log them. Don't like this, but need a
# cleaner solution

unlogged <- unlist(lapply(assay_data, function(x) max(x) > 20))

for (unlogged_expression_type in names(unlogged)[unlogged]) {
  if (unlogged_expression_type %in% names(assay_data)) {
    assay_data[[unlogged_expression_type]] <- log2(assay_data[[unlogged_expression_type]])
  }
}

# Create output paths

print("Creating output paths...")

png_outdir <- file.path(opt$outdir, "png")
html_outdir <- file.path(opt$outdir, "html")

for (od in c(png_outdir, html_outdir)) {
  dir.create(path = od, recursive = TRUE, showWarnings = FALSE)
}

# Take gene metadata from the raw matrix (if we assume the nf-core workflow there should be symbols)

gene_meta <- read_metadata(
  filename = opt$feature_metadata,
  id_col = opt$feature_id_col,
  sep = "\t"
)

# Add symbols to matrix rows for display purposes

assay_data <- lapply(assay_data, function(x) {
  rownames(x) <- paste(gene_meta[rownames(x), "gene_name"], "/", rownames(x))
  x
})

################################################
################################################
## Boxplots                                   ##
################################################
################################################

print("Writing boxplots...")

print("... interactive")
interactive_boxplot <- plotly_boxplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, "Set1"))
htmlwidgets::saveWidget(plotly::as_widget(interactive_boxplot), file.path(html_outdir, "boxplot.html"), selfcontained = TRUE)

print("... static")
static_boxplot <- ggplot_boxplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, "Set1"))
png(filename = file.path(png_outdir, "boxplot.png"), width = 800, height = 600)
static_boxplot
dev.off()

################################################
################################################
## Density plots                              ##
################################################
################################################

print("Writing density plots...")

print("...interactive")
interactive_densityplot <- plotly_densityplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, "Set1"))
htmlwidgets::saveWidget(plotly::as_widget(interactive_densityplot), file.path(html_outdir, "density.html"), selfcontained = TRUE)

print("... static")
static_densityplot <- ggplot_densityplot(assay_data, experiment = sample_metadata, colorby = opt$contrast_variable, expressiontype = "count per gene", palette = makeColorScale(2, "Set1"))
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
    palette = makeColorScale(length(unique(plotdata$colorby)), "Set1"),
    legend_title = prettifyVariablename(opt$contrast_variable),
    labels = plotdata$name,
    show_labels = TRUE
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

  print(paste0("...interactive (", d, "d)"))
  interactive_pcaplot <- do.call("plotly_scatterplot", plot_args)
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
  2^assay_data[[final_assay]][selectVariableGenes(matrix = assay_data[[final_assay]], ntop = opt$n_genes), ],
  sample_metadata[, opt$contrast_variable, drop = FALSE],
  colorby = opt$contrast_variable,
  cor_method = "spearman",
  plot_title = paste0("Sample clustering dendrogram, ", opt$n_genes, " most variable genes"),
  cluster_method = "ward.D2",
  palette = makeColorScale(length(unique(sample_metadata[[opt$contrast_variable]])), palette = "Set1")
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

mad_plot_args <- list(
  x = plotdata$group,
  y = plotdata$mad,
  color = plotdata$outlier,
  hline_thresholds = c("Outlier threshold" = -5),
  palette = makeColorScale(2, palette = "Set1"),
  legend_title = "Outlier status",
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
png(filename = file.path(png_outdir, "mad_correlation.png"), width = 800, height = 600)
print(static_madplot)
dev.off()
