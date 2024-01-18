#!/usr/bin/env Rscript

# Call shinyngs parsing functions to validate simple matrix inputs

library(optparse)

option_list <- list(
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
    c("-e", "--assay_files"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of TSV-format file expression matrix files."
  ),
  make_option(
    c("-c", "--contrasts_file"),
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
    c("-t", "--separator"),
    type = "character",
    default = "\t",
    help = "Consistent separator for re-written files."
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check mandatory

mandatory <-
  c(
    "sample_metadata",
    "assay_files"
  )

missing_args <- mandatory[!mandatory %in% names(opt)]
if (length(missing_args) > 0) {
  stop(paste("Missing mandatory arguments:", paste(missing_args, collapse = ", ")))
}

library(shinyngs)

# validate_inputs() just wraps the parsing functions of shinyng, used by e.g.
# eselistfromConfig(). These functions are good for ensuring the consistency of
# FOM (feaure/ observation matrix) data.

validated_parts <- validate_inputs(
  samples_metadata = opt$sample_metadata,
  features_metadata = opt$feature_metadata,
  assay_files = opt$assay_files,
  assay_names = opt$assay_names,
  contrasts_file = opt$contrasts_file,
  sample_id_col = opt$sample_id_col,
  feature_id_col = opt$feature_id_col,
  differential_results = opt$differential_results,
  pval_column = opt$pval_column,
  qval_column = opt$qval_column,
  fc_column = opt$fold_change_column,
  unlog_foldchanges = opt$unlog_foldchanges
)

# If an output path is provided we can re-write the data, ensuring consistency
# of output formatting

if (! is.null(opt$output_directory)){
  
  dir.create(opt$output_directory, showWarnings = FALSE, recursive = TRUE)
  
  # Write the files back, but using the supplied separator
  
  write_table <- function(x, infile, suffix, na = 'NA'){
    file_basename <- tools::file_path_sans_ext(basename(infile))
    outfile <- file.path(opt$output_directory, paste(file_basename, suffix, 'tsv', sep = '.'))
    
    print(paste("...... writing", outfile))
    write.table(x, file = outfile, sep = opt$separator, quote = FALSE, row.names = FALSE, na = na)
  }
  
  # Write back the sample sheet, feature metadata and contrasts
  
  print("Writing basic data...")
  for (infile in c('sample_metadata', 'feature_metadata', 'contrasts_file')){
    filename <- opt[[infile]]
    if ((! is.null(filename)) && filename %in% names(validated_parts)){
      write(paste("...", infile))

      # Write contrasts file with empty strings for NAs in blocking
      write_table(validated_parts[[filename]], filename, infile, na = ifelse(infile == 'contrasts_file', '', 'NA'))
    }
  }
  
  # Write back the matrices
  
  print("Writing matrices...")
  if ('assays' %in% names(validated_parts)){
    for (assay in names(validated_parts[['assays']])){
      mat <- validated_parts[['assays']][[assay]]
      
      # Add a column for row names
      mat <- data.frame(feature_name = rownames(mat), mat, check.names = FALSE)
      colnames(mat)[1] <- opt$feature_id_col
      
      write_table(mat, assay, 'assay')
    }
  }
  
  # Write back the simplified differential results (if supplied)
  
  if ('differential_stats' %in% names(validated_parts)){
    for (ds in names(validated_parts[['differential_stats']])){
      write_table(validated_parts[['differential_stats']][[ds]], ds)
    }
  }
  
}


