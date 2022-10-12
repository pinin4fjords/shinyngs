#!/usr/bin/env Rscript

# This should be integrated with

library(optparse)

option_list <- list(
  make_option(
    c("-t", "--title"),
    type = "character",
    default = "Default title",
    help = "Experiment title to show."
  ),
  make_option(
    c("-a", "--author"),
    type = "character",
    default = "My authors",
    help = "Author string to display."
  ),
  make_option(
    c("-r", "--description"),
    type = "character",
    default = "Look what  gone done",
    help = "Joe Bloggs."
  ),
  make_option(
    c("-s", "--sample_metadata"),
    type = "character",
    default = NULL,
    help = "CSV-format sample metadata file."
  ),
  make_option(
    c("-i", "--sample_id_col"),
    type = "character",
    default = "sample",
    help = "Column in sample metadata used as sample identifier. Should be used to name columns of expression matrices, and duplicate rows will be removed based on this column."
  ),
  make_option(
    c("-f", "--feature_metadata"),
    type = "character",
    default = NULL,
    help = "CSV-format feature (often gene) metadata file."
  ),
  make_option(
    c("-j", "--feature_id_col"),
    type = "character",
    default = "gene_id",
    help = "Column in feature metadata used as feature identifier. Should be used to name columns of expression matrices."
  ),
  make_option(
    c("-e", "--assay_files"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of TSV-format file expression matrix files."
  ),
  make_option(
    c("-x", "--assay_entity_name"),
    type = "character",
    default = "gene",
    help = "Name of type of thing represented in assays. Default: gene."
  ),
  make_option(
    c("-y", "--contrast_stats_assay"),
    type = "numeric",
    default = 1,
    help = "Integer indicating which element of --assay_files should be associated in displays with contrast statistics. Usually a normalised matrix useful for relating stats to assay values."
  ),
  make_option(
    c("-g", "--group_vars"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of variables in the sample metadata to use as grouping variables. Shinyngs will guess these variables by default."
  ),
  make_option(
    c("-c", "--contrast_file"),
    type = "character",
    default = NULL,
    help = "CSV-format contrast file with variable,reference and target in the first 3 columns."
  ),
  make_option(
    c("-d", "--differential_results"),
    type = "character",
    default = NULL,
    help = "Tab-separated files containing at least fold change and p value, one for each row of the contrast file."
  ),
  make_option(
    c("-k", "--fold_change_column"),
    type = "character",
    default = "log2FoldChange",
    help = "Column in differential results files holding fold changes."
  ),
  make_option(
    c("-u", "--unlog_foldchanges"),
    action = "store_true",
    default = FALSE,
    help = "Set this option if fold changes should be unlogged."
  ),
  make_option(
    c("-p", "--pval_column"),
    type = "character",
    default = "padj",
    help = "Column in differential results files holding p values."
  ),
  make_option(
    c("-q", "--qval_column"),
    type = "character",
    default = "padj",
    help = "Column in differential results files holding q values/ adjusted p values."
  ),
  make_option(
    c("-o", "--output_directory"),
    type = "character",
    default = NULL,
    help = "Serialized R object which can be used to generate a shiny app."
  ),
  make_option(
    c("-l", "--deploy_app"),
    action = "store_true",
    default = FALSE,
    help = "Set this option if fold changes should be unlogged."
  ),
  make_option(
    c("-b", "--shinyapps_account"),
    type = "character",
    default = NULL,
    help = "Account name for shinyapp deploment."
  ),
  make_option(
    c("-m", "--shinyapps_token"),
    type = "character",
    default = NULL,
    help = "Token for shinyapp deploment."
  ),
  make_option(
    c("-n", "--shinyapps_secret"),
    type = "character",
    default = NULL,
    help = "Secret for shinyapp deploment."
  ),
  make_option(
    c("-v", "--shinyapps_name"),
    type = "character",
    default = NULL,
    help = "App name for shinyapp deploment."
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check mandatory

mandatory <-
  c(
    "title",
    "author",
    "description",
    "sample_metadata",
    "sample_id_col",
    "feature_metadata",
    "feature_id_col",
    "assay_files",
    "assay_entity_name",
    "contrast_file",
    "differential_results",
    "output_directory",
    "contrast_stats_assay"
  )

missing_args <- mandatory[!mandatory %in% names(opt)]
if (length(missing_args) > 0) {
  stop(paste("Missing mandatory arguments:", paste(missing_args, collapse = ", ")))
}

library(shinyngs)

# Parse inputs

# Name assay data

assay_files <- unlist(strsplit(opt$assay_files, ","))
names(assay_files) <-
  unlist(lapply(assay_files, function(x) {
    prettifyVariablename(tools::file_path_sans_ext(gsub("_", " ", basename(x))))
  }))

# Contrasts

contrasts <- read.csv(file = opt$contrast_file)
contrast_stats_files <- strsplit(opt$differential_results, ",")
contrast_stats_assay <- opt$contrast_stats_assay
names(contrast_stats_files) <- names(assay_files)[contrast_stats_assay]

contrast_stats <- list()
contrast_stats[[opt$assay_entity_name]] <- lapply(contrast_stats_files, function(x) {
  list(
    "files" = x,
    "type" = "uncompiled",
    "feature_id_column" = opt$feature_id_col,
    "fc_column" = opt$fold_change_column,
    "pval_column" = opt$pval_column,
    "qval_column" = opt$qval_column,
    "unlog_foldchanges" = opt$unlog_foldchanges,
    "sep" = "\t"
  )
})

################################################
################################################
## Build the app                               ##
################################################
################################################

experiments <- list()
experiments[[opt$assay_entity_name]] <- list(
  "coldata" = list(
    "file" = opt$sample_metadata,
    "id" = opt$sample_id_col,
    "sep" = ","
  ),
  "annotation" = list(
    "file" = opt$feature_metadata,
    "id" = opt$feature_id_col,
    "sep" = "\t",
    "label" = "gene_name"
  ),
  "expression_matrices" = lapply(assay_files, function(x) {
    list(
      file = x,
      measure = "counts",
      sep = "\t"
    )
  })
)

shiny_config <- list(
  "title" = opt$title,
  "author" = opt$author,
  "group_vars" = opt$group_vars,
  "default_groupvar" = opt$group_vars[1],
  "experiments" = experiments,
  "contrasts" = list(
    "comparisons" = apply(contrasts, 1, function(x) {
      list(
        "Variable" = x[1],
        "Group.1" = x[2],
        "Group.2" = x[3]
      )
    }),
    "stats" = contrast_stats
  )
)

if (!is.null(opt$group_vars)) {
  opt$group_vars <- unlist(strsplit(opt$group_vars, ","))
  shiny_config[["group_vars"]] <- opt$group_vars
  shiny_config[["default_groupvar"]] <- opt$group_vars[1]
}

myesel <- eselistfromConfig(shiny_config)

# Write output

dir.create(opt$output_directory, showWarnings = FALSE)
saveRDS(myesel, file = file.path(opt$output_directory, "data.rds"))
writeLines(
  c(
    "library(shinyngs)",
    "library(markdown)",
    'esel <- readRDS("data.rds")',
    'app <- prepareApp("rnaseq", esel)',
    "shiny::shinyApp(app$ui, app$server)"
  ),
  file.path(opt$output_directory, "app.R")
)

# Broken with Conda install: shinyapps needs the package to have been
# installed via devtools, and conda doesn't do that

if (opt$deploy_app) {
  library(rsconnect)
  mandatory <- c(
    "shinyapps_account",
    "shinyapps_token",
    "shinyapps_secret",
    "shinyapps_name"
  )
  missing_args <- mandatory[!mandatory %in% names(opt)]
  if (length(missing_args) > 0) {
    stop(paste("Missing mandatory arguments for shinyapps deployment:", paste(missing_args, collapse = ", ")))
  }

  library(BiocManager)
  options(repos = BiocManager::repositories())
  rsconnect::setAccountInfo(name = opt$shinyapps_account, token = opt$shinyapps_token, secret = opt$shinyapps_secret)
  deployApp(appDir = opt$output_directory, appName = opt$shinyapps_name, forceUpdate = TRUE, launch.browser = FALSE)
}
