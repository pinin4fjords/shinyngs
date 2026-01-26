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
    default = NULL,
    help = "A description to display in the app."
  ),
  make_option(
    c("-m", "--report_markdown_file"),
    type = "character",
    default = NULL,
    help = "Path to file with descripion/ reporting in markdown. Alternative to 'description' for more extensive description content."
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
    help = "TSV-format feature (often gene) metadata file."
  ),
  make_option(
    c("-j", "--feature_id_col"),
    type = "character",
    default = "gene_id",
    help = "Column in feature metadata used as feature identifier. Should be used to name columns of expression matrices."
  ),
  make_option(
    c("-N", "--feature_name_col"),
    type = "character",
    default = "gene_name",
    help = "Column in feature metadata used as feature name/ label. Can be different to matrix column names."
  ),
  make_option(
    c("-n", "--diff_feature_id_col"),
    type = "character",
    metavar = "string",
    help = "Differential file column name containing feature identifiers.",
    default = "gene_id"
  ),
  make_option(
    c("-e", "--assay_files"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of CSV or TSV-format file expression matrix files."
  ),
  make_option(
    c("-w", "--assay_names"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of names of same length as --assay-files."
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
    default = NULL,
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
    help = "Comma-separated list of CSV or TSV-format files containing at least fold change and p value, one for each row of the contrast file."
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
    "--enrichment_gene_sets",
    type = "character",
    default = NULL,
    help = "Comma-separated list of the GMT files used in the enrichment analyses."
  ),
  make_option(
    "--enrichment_filename_template",
    type = "character",
    default = NULL,
    help = "Template of filenames containing enrichment results. For instance '{contrast_name}_enrichment_for_{geneset_type}.tsv' or, if up and down regulated results are provided separately '{contrast_name}.{geneset_type}.gsea_report_for_{target|reference}.tsv'. '{contrast_name}', '{geneset_type}', and optionally '{target|reference}' are substituted dynamically for each contrast and geneset type. If not given, no enrichment results are included"
  ),
  make_option(
    "--enrichment_skip_missing",
    action = "store_true",
    default = FALSE,
    help = "Ignore if any gene set enrichment result for any contrast is missing"
  ),
  make_option(
    "--enrichment_gene_type_id",
    type = "character",
    default = "gene_name",
    help = "Gene identifier in the enrichment gene sets. Use this to specify that the gmt files represent genes with the gene name or an entrez id"
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
    c("-v", "--shinyapps_name"),
    type = "character",
    default = NULL,
    help = "App name for shinyapp deploment."
  ),
  make_option(
    c("--log2_assays"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of assay_names to which to apply log2. Alternatively, comma-separated list of positive integers indicating which assays to log (1-based!). If not specified, the script will guess the log status based on the maximum value of the input data. If empty string, will not apply log2."
  ),
  make_option(
    c("--log2_guessing_threshold"),
    type = "integer",
    metavar = "integer",
    help = "Magnitude used to guess log status.",
    default = 30
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check mandatory

mandatory <-
  c(
    "title",
    "author",
    "sample_metadata",
    "sample_id_col",
    "feature_metadata",
    "feature_id_col",
    "diff_feature_id_col",
    "assay_files",
    "assay_entity_name",
    "output_directory",
    "contrast_stats_assay"
  )

missing_args <- mandatory[!mandatory %in% names(opt)]
if (length(missing_args) > 0) {
  stop(paste("Missing mandatory arguments:", paste(missing_args, collapse = ", ")))
}

if (opt$deploy_app) {
  
  # Check we have what we need for an app deployment
  
  mandatory <- c(
    "shinyapps_account",
    "shinyapps_name"
  )
  missing_args <- mandatory[!mandatory %in% names(opt)]
  if (length(missing_args) > 0) {
    stop(paste("Missing mandatory arguments for shinyapps deployment:", paste(missing_args, collapse = ", ")))
  }
  
  mandatory <- c(
    'SHINYAPPS_SECRET',
    'SHINYAPPS_TOKEN'
  )
  missing_secrets <- mandatory[!mandatory %in% names(Sys.getenv())]
  if (length(missing_secrets) > 0) {
    stop(paste("Environment variables not defined for shinyapps deployment:", paste(missing_secrets, collapse = ", ")))
  }
  
  # The app deployment will often fail if BioC packages are out of date, but we
  # can't assume we have access to the system R directories. So re-install
  # outdated ones to a local dir before any important library calls.
  
  print("Updating BioC packages as will be required for shinyapps.io deployment")
  
  library(BiocManager)
  options(repos = BiocManager::repositories())
  ood <- data.frame(BiocManager::valid()$out_of_date)
  ood_packages <- ood[grep('bioconductor', ood$Repository), 'Package']
  
  dir.create('libs', showWarnings = FALSE) 
  .libPaths('libs')
  
  BiocManager::install(ood_packages, update = TRUE, ask = FALSE, lib = 'libs')
}

library(shinyngs)

# Name assay data

assay_files <-
  stringsToNamedVector(
    elements_string = opt$assay_files,
    opt$assay_names,
    simplify_files = TRUE,
    prettify_names = TRUE
  )

# Contrasts

contrast_stats_files <- strsplit(opt$differential_results, ",")
contrast_stats_assay <- opt$contrast_stats_assay

# Pick last assay by default to relate the stats to

if (is.null(contrast_stats_assay)){
    contrast_stats_assay <- length(assay_files)
}
names(contrast_stats_files) <- names(assay_files)[contrast_stats_assay]

contrast_stats <- list()
contrast_stats[[opt$assay_entity_name]] <- lapply(contrast_stats_files, function(x) {
  list(
    "files" = x,
    "type" = "uncompiled",
    "feature_id_column" = opt$diff_feature_id_col,
    "fc_column" = opt$fold_change_column,
    "pval_column" = opt$pval_column,
    "qval_column" = opt$qval_column,
    "unlog_foldchanges" = opt$unlog_foldchanges
  )
})

# Enrichment results:
# To show enrichment results we need:
# - The contrasts file opt$contrast_file
# - The gmt enrichment files used
# - The actual enrichment results
#

if (!is.null(opt$enrichment_filename_template)) {
  # Ensure we have gene sets and contrasts
  if (is.null(opt$contrast_file)) {
    stop("When --enrichment_filename_template is given, --contrast_file is required")
  }
  if (is.null(opt$enrichment_gene_sets)) {
    stop("When --enrichment_filename_template is given, --enrichment_gene_sets is required")
  }

  contrasts_df <- read_metadata(opt$contrast_file)
  genesets_files <- simpleSplit(opt$enrichment_gene_sets)
  names(genesets_files) <- tools::file_path_sans_ext(basename(genesets_files))

  gene_set_analyses <- list(
    lapply(setNames(nm=names(genesets_files)), function(geneset_type) {
      lapply(setNames(nm=contrasts_df$id), function(contrast_name) {
        ctrst_info <- as.list(contrasts_df[contrasts_df$id == contrast_name,])
        # Two types of opt$enrichment_filename_template are supported:
        # - The template points to a single file for each contrast and geneset_type
        # - The template points to two files for each contrast and geneset_type. In
        #   this case, one file is for the up-regulated results and another file is
        #   for the down-regulated results.
        #
        # The template may look like:
        # "gsea_for_{contrast_name}_using_{geneset_type}_for_{target|reference}.tsv"
        # Which for a contrast "disease_vs_healthy" with target="disease" and reference="healthy"
        # and geneset_type "geo_bp", would result in shinyngs expecting two files named:
        # up: "gsea_for_disease_vs_healthy_using_geo_bp_for_disease.tsv"
        # down: "gsea_for_disease_vs_healthy_using_geo_bp_for_healthy.tsv"
        #
        # If the template does not include {target|reference} we assume there is
        # a single file per contrast and geneset type, for instance:
        # "gsea_for_{contrast_name}_using_{geneset_type}_results.tsv"
        # becomes:
        # "gsea_for_disease_vs_healthy_using_geo_bp_results.tsv"
        if (grepl("target\\|reference", opt$enrichment_filename_template)) {
          up_file <- build_enrichment_path(opt$enrichment_filename_template, ctrst_info, geneset_type, "up")
          down_file <- build_enrichment_path(opt$enrichment_filename_template, ctrst_info, geneset_type, "down")
          if (file.exists(up_file) && file.exists(down_file)) {
            list("up" = up_file, "down" = down_file)
          } else {
            if (opt$enrichment_skip_missing) {
              NULL
            } else {
              stop(sprintf("both enrichment files should exist: %s and %s", up_file, down_file))
            }
          }
        } else {
          enrichment_file <- build_enrichment_path(opt$enrichment_filename_template, ctrst_info, geneset_type)
          if (file.exists(enrichment_file)) {
            enrichment_file
          } else {
            if (opt$enrichment_skip_missing) {
              NULL
            } else {
              stop(sprintf("enrichment file: %s does not exist", enrichment_file))
            }
          }
        }
      })
    })
  )
  names(gene_set_analyses) <- names(assay_files)[contrast_stats_assay]
} else {
  gene_set_analyses <- list()
  genesets_files <- list()
}



################################################
################################################
## Build the app                              ##
################################################
################################################

experiments <- list()
experiments[[opt$assay_entity_name]] <- list(
  "coldata" = list(
    "file" = opt$sample_metadata,
    "id" = opt$sample_id_col
  ),
  "annotation" = list(
    "file" = opt$feature_metadata,
    "id" = opt$feature_id_col,
    "label" = opt$feature_name_col
  ),
  "expression_matrices" = lapply(assay_files, function(x) {
    list(
      file = x,
      measure = "counts"
    )
  }),
  "gene_set_analyses" = gene_set_analyses
)

shiny_config <- list(
  "title" = opt$title,
  "author" = opt$author,
  "group_vars" = opt$group_vars,
  "default_groupvar" = opt$group_vars[1],
  "experiments" = experiments
)

if (! is.null(opt$contrast_file)){
  shiny_config$contrasts = list(
    "comparisons_file" = opt$contrast_file,
    "stats" = contrast_stats
  )  
}

if (!is.null(opt$group_vars)) {
  opt$group_vars <- simpleSplit(opt$group_vars, ",")
  shiny_config[["group_vars"]] <- opt$group_vars
  shiny_config[["default_groupvar"]] <- opt$group_vars[1]
}

if (!is.null(opt$description)) {
  shiny_config[['description']] <- opt$description
} else if (!is.null(opt$report_markdown_file)) {
  shiny_config[['report']] <- opt$report_markdown_file
}

if (length(genesets_files) > 0) {
  shiny_config[["gene_set_id_type"]] <- opt$enrichment_gene_type_id
  shiny_config[["gene_sets"]] <- genesets_files
}


myesel <- eselistfromConfig(
  shiny_config,
  log2_assays = opt$log2_assays,
  log2_threshold = opt$log2_guessing_threshold
)

# Write output

dir.create(opt$output_directory, showWarnings = FALSE, recursive = TRUE)
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

# If deployment has been indicated, try to do that. Needs SHINYAPPS_SECRET AND
# SHINYAPPS_TOKEN to be set in the environment

if (opt$deploy_app) {
  library(rsconnect)

  options(BiocManager.check_repositories = FALSE)
  rsconnect::setAccountInfo(name = opt$shinyapps_account, token = Sys.getenv('SHINYAPPS_TOKEN'), secret = Sys.getenv('SHINYAPPS_SECRET'))
  deployApp(appDir = opt$output_directory, appName = opt$shinyapps_name, forceUpdate = TRUE, launch.browser = FALSE)
}
