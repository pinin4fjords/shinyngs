#!/usr/bin/env Rscript

library(optparse)
library(shinyngs)

option_list <- list(
  make_option(
    c("-t", "--title"),
    type = "character",
    default = 'Default title',
    help = "Experiment title to show."
  ),
  make_option(
    c("-a", "--author"),
    type = "character",
    default = 'My authors',
    help = "Author string to display."
  ),
  make_option(
    c("-r", "--description"),
    type = "character",
    default = 'Look what  gone done',
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
    default = 'sample',
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
    default = 'gene_id',
    help = "Column in feature metadata used as feature identifier. Should be used to name columns of expression matrices."
  ),
  make_option(
    c("-e", "--assay_files"),
    type = "character",
    default = NULL,
    help = "Comma-separated list of TSV-format file expression matrix files."
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
    c("-d", "--differential-results"),
    type = "character",
    default = NULL,
    help = "Tab-separated files containing at least fold change and p value, one for each row of the contrast file."
  ),
  make_option(
    c("-o", "--output_file"),
    type = "character",
    default = NULL,
    help = "Serialized R object which can be used to generate a shiny app."
  )
)

opt_parser <- OptionParser(option_list = option_list)
opt        <- parse_args(opt_parser)

# Parse inputs

if (! is.null(opt$group_vars)){
  opt$group_vars <- unlist(strsplit(opt$group_vars, ','))
}


################################################
################################################
## Functions                                  ##
################################################
################################################

# Read metadata file

read_metadata <- function(filename, id_col, sep = ','){
  if (! file.exists(filename)){
    stop(paste('Metadata file', filename, 'does not exist.'))
  }else{
    metadata <- read.delim(filename, sep = sep, check.names = FALSE, header = TRUE)
  }
  
  if (! id_col %in% colnames(metadata)){
    stop(paste0("Metadata ID column (", id_col, ") does not exist in metadata.", paste(colnames(metadata), sep = ',')))
  }
  
  metadata <- metadata[match(unique(metadata[[id_col]]), metadata[[id_col]]), ]
  rownames(metadata) <- metadata[[id_col]]
  return(metadata)
}

read_matrix <- function(matrix_file){
  matrix_data <- read.table(matrix_file, check.names = FALSE, header = TRUE)
  
  if (any(! rownames(sample_metadata) %in% colnames(matrix_data))){
    stop(paste("Some sample metadata names are absent from the matrix in", matrix_file))
  }
  if (any(! rownames(feature_metadata) %in% rownames(matrix_data))){
    rownames(matrix_data) <- matrix_data[,1]
    if (any(! rownames(feature_metadata) %in% rownames(matrix_data))){
      stop(paste("Some feature metadata names are absent from the matrix in", matrix_file))
    }
  }
  
  return (matrix_data[,rownames(sample_metadata)])
}

################################################
################################################
## Read data                                  ##
################################################
################################################

# Read gene and sample metadata

sample_metadata <- read_metadata(opt$sample_metadata, id_col = opt$sample_id_col)
feature_metadata <- read_metadata(opt$feature_metadata, id_col = opt$feature_id_col, sep = "\t")

# Read assay data

assay_files <- unlist(strsplit(opt$assay_files, ','))
names(assay_files) <-
  unlist(lapply(assay_files, function(x)
    prettifyVariablename(tools::file_path_sans_ext(gsub('_', ' ', x)))))

# Read and check gene metadata

myassays <- lapply(assay_files, function(x)
    read_matrix(x))

################################################
################################################
## Build the app                               ##
################################################
################################################

myese <- ExploratorySummarizedExperiment(
  assays = SimpleList(
    myassays
  ),
  colData = DataFrame(sample_metadata),
  annotation <- feature_metadata,
  idfield = 'gene_id',
  labelfield = "gene_name"
)

myesel <- ExploratorySummarizedExperimentList(
  eses = list(expression = myese),
  title = opt$title,
  author = opt$author,
  description = opt$description,
  group_vars = opt$group_vars
)

app <- prepareApp("rnaseq", myesel)
shiny::shinyApp(app$ui, app$server)
