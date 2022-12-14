#!/usr/bin/env Rscript

library(optparse)

################################################
################################################
## Argument parsing                           ##
################################################
################################################

option_list <- list(
  make_option(
    c("-d", "--differential_file"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "TSV file containing a tabe of differential analysis outputs."
  ),
  make_option(
    c("-e", "--feature_metadata"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "TSV file containing feature identifiers and symbols."
  ),
  make_option(
    c("-g", "--feature_id_col"),
    type = "character",
    metavar = "string",
    help = "Count file column containing feature identifiers.",
    default = "gene_id"
  ),
  make_option(
    c("-m", "--feature_name_col"),
    type = "character",
    metavar = "string",
    help = "Count file column containing feature names.",
    default = "gene_name"
  ),
  make_option(
    c("-o", "--outdir"),
    type = "character",
    default = NULL,
    metavar = "path",
    help = "Output directory."
  ),
  make_option(
    c("-f", "--fold_change_col"),
    type = "character",
    metavar = "string",
    help = "Differential file column name containing (log2) fold change values.",
    default = "log2FoldChange"
  ),
  make_option(
    c("-q", "--p_value_column"),
    type = "character",
    metavar = "string",
    help = "Differential file column name containing p values to plot.",
    default = "padj"
  ),
  make_option(
    c("-n", "--diff_feature_id_col"),
    type = "character",
    metavar = "string",
    help = "Differential file column name containing feature identifiers.",
    default = "gene_id"
  ),
  make_option(
    c("-c", "--fold_change_threshold"),
    type = "double",
    metavar = "float",
    help = "Lower fold change threshold for differential expression.",
    default = 1
  ),
  make_option(
    c("-u", "--p_value_threshold"),
    type = "double",
    metavar = "float",
    help = "p value threshold for differential expression.",
    default = 0.05
  ),
  make_option(
    c("-r", "--reference_level"),
    type = "character",
    metavar = "string",
    help = "For differential plot annotation. Negative fold changes will be annotated as being higher in this group. "
  ),
  make_option(
    c("-t", "--treatment_level"),
    type = "character",
    metavar = "string",
    help = "For differential plot annotation. Postitive fold changes will be annotated as being higher in this group. "
  ),
  make_option(
    c("-s", "--unlog_foldchanges"),
    action = "store_true",
    default = FALSE,
    help = "Set this option if fold changes should be unlogged."
  ),
  make_option(
    c("-x", "--write_html"),
    action = "store_true",
    default = FALSE,
    help = "Set this option to produce HTML outputs as well as PNGs."
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt        <- parse_args(opt_parser)

for (input in c('differential_file', 'feature_metadata', 'outdir', 'reference_level', 'treatment_level')){
  if (is.null(opt[[input]])) {
    print_help(opt_parser)
    stop(paste0("Please provide a ", input), call. = FALSE)
  }
}

# Create output paths

print("Creating output paths...")

png_outdir <- file.path(opt$outdir, 'png')
html_outdir <- file.path(opt$outdir, 'html') 

dir.create(path = png_outdir, recursive = TRUE, showWarnings = FALSE)

if (opt$write_html){
    dir.create(path = html_outdir, recursive = TRUE, showWarnings = FALSE)
}

################################################
################################################
## Library loading                            ##
################################################
################################################

library(shinyngs)
library(plotly)

################################################
################################################
## Plotting                                   ##
################################################
################################################

differential <- read_differential(
    filename = opt$differential_file,
    feature_id_column = opt$diff_feature_id_col,
    pval_column = opt$p_value_column,
    qval_column = opt$p_value_column,
    fc_column = opt$fold_change_col,
    unlog_foldchanges = opt$unlog_foldchanges
)

differential <- subset(differential, (! is.na(differential[[opt$fold_change_col]])) & (! is.na(differential[[opt$p_value_column]])) )

feature_metadata <-
  read_metadata(
    opt$feature_metadata,
    id_col = opt$feature_id_col,
    stringsAsFactors = FALSE
  )

# Label features with symbol as well as identifier

differential$label <- feature_metadata[differential[[opt$feature_id_col]],opt$feature_name_col]

# We'll color by whether features are differential according to supplied thresholds

differential$differential_status <- FALSE
differential$differential_status[abs(differential[[opt$fold_change_col]]) > opt$fold_change_threshold & differential[opt$p_value_column] < opt$p_value_threshold] <- TRUE

# Define the thresholds we'll draw

hline_thresholds = vline_thresholds = list()
hline_thresholds[[paste(opt$p_value_column, '=', opt$p_value_threshold)]] = -log10(opt$p_value_threshold)
vline_thresholds[[paste(opt$fold_change_col, '<-', opt$fold_change_threshold)]] = -opt$fold_change_threshold
vline_thresholds[[paste(opt$fold_change_col, '>', opt$fold_change_threshold)]] = opt$fold_change_threshold

plot_args <- list(
  x = log2(differential[[opt$fold_change_col]]),
  y = -log10(differential[[opt$p_value_column]]),
  colorby = differential$differential_status,
  ylab = paste("-log(10)", opt$p_value_column),
  xlab = xlabel <- paste("higher in", opt$reference_level, "          <<", opt$fold_change_col, ">>           higher in", opt$treatment_level) ,
  labels = differential$label,
  hline_thresholds = hline_thresholds,
  vline_thresholds = vline_thresholds,
  show_labels = FALSE,
  legend_title = "Differential status",
  palette = makeColorScale(2, 'Set1')
)

print("Writing volcano plots...")

print("... static")

# Let's show labels for significant features in the static plot

plot_args$show_labels <- TRUE
plot_args$labels[! differential$differential_status] <- NA

png(filename = file.path(png_outdir, "volcano.png"), width=800, height=600)
do.call(static_scatterplot, plot_args)
dev.off()

if (opt$write_html){
    print("...interactive")
    interactive_volcanoplot <- do.call(plotly_scatterplot, plot_args)
    htmlwidgets::saveWidget(as_widget(interactive_volcanoplot), file.path(html_outdir, "volcano.html"), selfcontained = TRUE)
}
