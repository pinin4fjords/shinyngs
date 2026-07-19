#' Build an ExploratorySummarisedExperimentList from a YAML description
#'
#' Building ExploratorySummarisedExperimentList objects can be a bit fiddly.
#' This function makes automates object construction based on a descriptor
#' in yaml format.
#'
#' For a simple study with one 'experiement' for Gene-level results, and three
#' 'assays' describing raw, filtered and normalised expression you might make
#' a YAML like:
#' ```
#' title: My RNA seq experiment
#' author: Joe Blogs
#' report: report.md
#' group_vars:
#'   - Group
#'   - Replicate
#' default_groupvar: Group
#' experiments:
#'   Gene:
#'     coldata:
#'       file: my.experiment.csv
#'       id: External
#'     annotation:
#'       file: my.annotation.csv
#'       id: gene_id
#'       entrez: ~
#'       label: gene_id
#'     expression_matrices:
#'       Raw:
#'         file: raw_counts.csv
#'         measure: counts
#'       Filtered:
#'         file: filtered_counts.csv
#'         measure: Counts per million
#'       Normalised:
#'         file: normalised_counts.csv
#'         measure: Counts per million
#'     read_reports:
#'       read_attrition: read_attrition.csv
#' contrasts:
#'   comparisons:
#'   - Variable: Group
#'     Group.1: control
#'     Group.2: TreatmentA
#'   - Variable: Group
#'     Group.1: control
#'     Group.2: TreatmentB
#' contrast_stats:
#'   Gene:
#'     Normalised:
#'       pvals: pvals.csv
#'       qvals: qvals.csv
#' ```
#' @param configfile A YAML-format config file describing the data to be
#'   compiled into an ExploratorySummarizedExperimentList object
#'
#' @import yaml
#' @return out An ExploratorySummarizedExperimentList object suitable for passing to \code{\link{prepareApp}}
#' @export
#'
#' @md
#' @examples
#' eselist <- eselistFromYAML("my.yaml")
#'
eselistFromYAML <- function(configfile) {
  config <- yaml::yaml.load_file(configfile)

  eselistFromList(config)
}

#' Reads gene enrichment files
#' @noRd
#' @param contrast_spec One of:
#' - \code{NULL} (meaning no enrichment was analyzed for that contrast)
#' - a path to a file (e.g. the table output from roast)
#' - a named list with elements "up" and "down" with paths to files (e.g.
#'  corresponding to gsea up-regulated and down-regulated output tables).
#'
#'  The two tables from GSEA output will be combined into a single data frame. A column "Direction" with
#'  values "Up" and "Down" will be added.
#'
#' @returns A data frame with the file contents (or \code{NULL})
#'
read_enrichment_file <- function(contrast_spec) {
  # contrast_spec may be one file name or two file names (up and down), or NULL
  if (is.null(contrast_spec) || length(contrast_spec) == 0) {
    return(NULL)
  }

  read_one <- function(path) {
    read.csv(path,
      sep = getSeparator(path), check.names = FALSE,
      row.names = 1
    )
  }

  if (length(contrast_spec) == 1) {
    return(read_one(contrast_spec))
  }

  if (length(contrast_spec) == 2) {
    # This is useful for GSEA output, that splits up and down in two tsv files.
    # We read both files and set the direction
    up <- read_one(contrast_spec[["up"]])
    if (nrow(up) > 0) {
      up$Direction <- "Up"
    }
    down <- read_one(contrast_spec[["down"]])
    if (nrow(down) > 0) {
      down$Direction <- "Down"
    }
    return(rbind(up, down))
  }

  stop("gene_set_analyses should have zero, one or two contrast files per gene_set_type")
}

#' Build an ExploratorySummarisedExperimentList from a description provided in a list
#'
#' @param config Hierachical named list with input components. See \code{eselistFromYAML} for detail.
#' @param log2_threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param log2_assays A string parameter that can be NULL, empty, or a non-empty string.
#'                     If NULL: log2 transformation will be guessed based on input assays.
#'                     If empty: no log2 transformation will be applied.
#'                     If non-empty: log2 transformation will be applied unconditionally to specified assays.
#'
#' @return out An ExploratorySummarizedExperimentList object suitable for passing to \code{\link{prepareApp}}
#' @export
#'
#' @examples
#' sample_metadata_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(sample = paste0("s", 1:4), condition = rep(c("treated", "control"), each = 2)),
#'   sample_metadata_file, row.names = FALSE
#' )
#' feature_metadata_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(gene_id = c("ENSG1", "ENSG2", "ENSG3"), gene_name = c("GeneA", "GeneB", "GeneC")),
#'   feature_metadata_file, row.names = FALSE
#' )
#' mat <- matrix(1:12, nrow = 3, dimnames = list(c("ENSG1", "ENSG2", "ENSG3"), paste0("s", 1:4)))
#' matrix_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
#'   matrix_file, row.names = FALSE
#' )
#' differential_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(
#'     gene_id = rownames(mat), log2FoldChange = c(1.2, -0.5, 2.1),
#'     pvalue = c(0.01, 0.2, 0.001), padj = c(0.02, 0.3, 0.005)
#'   ),
#'   differential_file, row.names = FALSE
#' )
#'
#' config <- list(
#'   title = "Example study",
#'   author = "An Author",
#'   experiments = list(
#'     rnaseq = list(
#'       coldata = list(file = sample_metadata_file, id = "sample"),
#'       annotation = list(file = feature_metadata_file, id = "gene_id", label = "gene_name"),
#'       expression_matrices = list(expression = list(file = matrix_file, measure = "Counts"))
#'     )
#'   ),
#'   contrasts = list(
#'     comparisons = list(c("condition", "treated", "control")),
#'     stats = list(
#'       rnaseq = list(
#'         expression = list(
#'           type = "uncompiled",
#'           files = list(differential_file),
#'           feature_id_column = "gene_id",
#'           fc_column = "log2FoldChange",
#'           pval_column = "pvalue",
#'           qval_column = "padj",
#'           fold_change_scale = "log2"
#'         )
#'       )
#'     )
#'   )
#' )
#' eselistfromConfig(config, log2_assays = "")
#'
eselistfromConfig <-
  function(config, log2_assays, log2_threshold = 30) {
    # 'Experiments' are sets of results from a common set of samples

    experiments <- config$experiments

    # Establish ordering. Ordering of YAML file shouldn't be relied upon

    if ("experiment_order" %in% names(config)) {
      experiment_order <- config$experiment_order
    } else {
      experiment_order <- names(experiments)
    }

    experiments <- experiments[experiment_order]

    # Make the basic objects

    message("Constructing ExploratorySummarizedExperiments")

    expsumexps <- lapply(structure(names(experiments), names = names(experiments)), function(expname) {
      exp <- experiments[[expname]]

      # Read feature metadata

      colData <- read_metadata(
        filename = exp$coldata$file,
        id_col = exp$coldata$id,
      )
      annotation <-
        read_metadata(
          exp$annotation$file,
          id_col = exp$annotation$id
        )

      # Read the expression data

      assays <- rev(lapply(exp$expression_matrices, function(mat) {
        message(paste("Reading", mat$file))
        read_matrix(
          mat$file,
          sample_metadata = colData,
          feature_metadata = annotation,
          row.names = 1
        )
      }))

      # If specified, ensure data is unlogged before it's loaded

      assays <- cond_log2_transform_assays(
        assays,
        log2_assays = log2_assays,
        threshold = log2_threshold,
        prettify_names = TRUE,
        reverse = TRUE,
        invert_assays = TRUE
      )

      # Apply ordering if provided

      if ("assay_order" %in% names(exp)) {
        assay_order <- exp$assay_order
      } else {
        assay_order <- names(assays)
      }
      assays <- assays[assay_order]

      # Add contrast_stats where available.

      contrast_stats <- list()
      if (expname %in% names(config$contrasts$stats)) {
        contrast_stats <- lapply(config$contrasts$stats[[expname]], function(assaytests) {
          # 'Uncompiled' means that stats are still stored in separate files for
          # each contrast, as they might come from DESeq etc. We just have to
          # separate them and compile ourselves.

          if ("type" %in% names(assaytests) && assaytests$type == "uncompiled") {
            fold_change_scale <- assaytests$fold_change_scale
            if (is.null(fold_change_scale)) {
              # Back-compat with configs written before fold_change_scale existed
              fold_change_scale <- if (isTRUE(assaytests$unlog_foldchanges)) "log2" else "auto"
            }

            compile_contrast_data(
              differential_stats_files = assaytests$files,
              feature_id_column = assaytests$feature_id_column,
              fc_column = assaytests$fc_column,
              pval_column = assaytests$pval_column, qval_column = assaytests$qval_column,
              fold_change_scale = fold_change_scale
            )
          } else {
            lapply(assaytests, function(at) {
              message(paste("Reading test stats file", at))
              read.csv(at, row.names = 1, header = FALSE)
            })
          }
        })
      }

      # Basic list to pass to object creation

      ese_list <- list(
        assays = assays,
        colData = colData,
        annotation = annotation,
        idfield = exp$annotation$id,
        entrezgenefield = exp$annotation$entrez,
        labelfield = exp$annotation$label,
        assay_measures = lapply(exp$expression_matrices, function(mat) {
          mat$measure
        }),
        contrast_stats = contrast_stats
      )

      if ("read_reports" %in% names(exp)) {
        ese_list$read_reports <- lapply(exp$read_reports, function(rrfile) read.csv(rrfile, row.names = 1, check.names = FALSE))
      }

      if ("gene_set_analyses" %in% names(exp)) {
        ese_list$gene_set_analyses <- lapply(exp$gene_set_analyses, function(assay) {
          lapply(assay, function(gene_set_type) {
            lapply(gene_set_type, read_enrichment_file)
          })
        })

        # Drop gene set types and assays with no results, but keep every
        # contrast position (NULLs included) so that downstream indexing by
        # contrast number stays aligned with the containing contrasts.
        ese_list$gene_set_analyses <- drop_empty_gene_set_analyses(ese_list$gene_set_analyses)
        # The constructor reconciles/validates the tool spec against the tables.
        ese_list$gene_set_analyses_tool <- exp$gene_set_analyses_tool
      }

      do.call(ExploratorySummarizedExperiment, ese_list)
    })

    # Parse contrasts if they weren't provided as a list directly

    if ("comparisons_file" %in% names(config$contrasts)) {
      config$contrasts$comparisons <-
        read_contrasts(config$contrasts$comparisons_file,
          colData(expsumexps[[1]]),
          convert_to_list = TRUE
        )
    }

    # Check that number of differential results sets is equal to number of contrasts

    for (ese in expsumexps) {
      if (ncol(ese@contrast_stats[[1]]$fold_changes) != length(config$contrasts$comparisons)) {
        stop(paste0("Number of supplied contrasts (", length(config$contrasts$comparisons), ") not equal to the number of sets of differential statistics supplied (", ncol(ese@contrast_stats[[1]]$fold_changes), ")"))
      }
    }

    message("Creating ExploratorySummarizedExperimentList")

    eselist_args <- list(
      expsumexps,
      title = config$title,
      author = config$author,
      group_vars = config$group_vars,
      default_groupvar = config$default_groupvar,
      contrasts = lapply(config$contrasts$comparisons, function(x) unlist(x))
    )

    # Optional things

    if ("static_pdf" %in% names(config)) {
      eselist_args$static_pdf <- config$static_pdf
    }

    # If 'report' is specified with assume a mardown document to be parsed. Otherwise just text

    if ("report" %in% names(config)) {
      eselist_args$description <- as.character(includeMarkdown(config$report))
    } else if ("description" %in% names(config)) {
      eselist_args$description <- config$description
    }

    if ("url_roots" %in% names(config)) {
      eselist_args$url_roots <- config$url_roots
    }

    if ("gene_set_id_type" %in% names(config) && "gene_sets" %in% names(config)) {
      eselist_args$gene_set_id_type <- config$gene_set_id_type
      eselist_args$gene_sets <- lapply(config$gene_sets, read_gmt)
    }

    if ("ensembl_species" %in% names(config)) {
      eselist_args$ensembl_species <- config$ensembl_species
    }

    eselist <- do.call(ExploratorySummarizedExperimentList, eselist_args)

    eselist
  }

#' Drop empty gene set types and assays from a gene_set_analyses structure
#' @noRd
#' @param gene_set_analyses A three-level nested list keyed by assay, gene set
#' type and contrast. Contrast entries may be \code{NULL} where no enrichment
#' result was supplied.
#'
#' @return The same structure with gene set types that hold no results, and
#' assays left with no gene set types, removed. Contrast positions are kept
#' intact (including \code{NULL}s) so that indexing by contrast number stays
#' aligned with the containing contrasts.
drop_empty_gene_set_analyses <- function(gene_set_analyses) {
  gene_set_analyses <- lapply(gene_set_analyses, function(assay) {
    Filter(function(gene_set_type) any(!vapply(gene_set_type, is.null, logical(1))), assay)
  })
  Filter(function(assay) length(assay) > 0, gene_set_analyses)
}

#' Read a GMT-format gene set file
#'
#' Parses a plain-text \code{.gmt} file (one gene set per line: set name,
#' description, then tab-separated gene identifiers - the format used by
#' MSigDB and similar resources) into a named list of character vectors, one
#' per gene set. Blank lines are ignored.
#'
#' @param file Path to a \code{.gmt} file
#'
#' @return A named list of character vectors of gene identifiers, one per
#' gene set, named after the set name in the file's first column.
#'
#' @export
#'
#' @examples
#' gmt_file <- tempfile(fileext = ".gmt")
#' writeLines(c(
#'   "SET1\tdescription\tGeneA\tGeneB\tGeneC",
#'   "SET2\tdescription\tGeneD\tGeneE"
#' ), gmt_file)
#' read_gmt(gmt_file)
#'
read_gmt <- function(file) {
  lines <- readLines(file)
  lines <- lines[nzchar(lines)]

  fields <- strsplit(lines, "\t")
  set_names <- vapply(fields, `[`, character(1), 1)

  duplicate_names <- unique(set_names[duplicated(set_names)])
  if (length(duplicate_names) > 0) {
    stop("Duplicate gene set name(s) in ", file, ": ", paste(duplicate_names, collapse = ", "))
  }

  gene_sets <- lapply(fields, function(x) x[-c(1, 2)])
  names(gene_sets) <- set_names

  gene_sets
}

#' Read an expression matrix file and match to specified samples and features
#'
#' @param matrix_file Matrix file
#' @param sample_metadata Data frame of sample metadata
#' @param feature_metadata Data frame of feature metadata
#' @param sep Separator in matrix file
#' @param row.names Matrix column number or name containing feature identifiers
#'
#' @return Numeric matrix
#' @export
#'
#' @examples
#' mat <- matrix(1:12, nrow = 3,
#'   dimnames = list(paste0("gene", 1:3), paste0("s", 1:4)))
#' matrix_file <- tempfile(fileext = ".tsv")
#' write.table(
#'   data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
#'   matrix_file, sep = "\t", row.names = FALSE, quote = FALSE
#' )
#' sample_metadata <- data.frame(
#'   condition = rep(c("treated", "control"), each = 2),
#'   row.names = paste0("s", 1:4)
#' )
#' read_matrix(matrix_file, sample_metadata)
#'
read_matrix <- function(matrix_file, sample_metadata, feature_metadata = NULL, sep = NULL, row.names = 1) {
  if (!file.exists(matrix_file)) {
    stop(paste("Matrix file", matrix_file, "does not exist."))
  }

  if (is.null(sep)) {
    sep <- getSeparator(matrix_file)
  }
  matrix_data <-
    read.delim(
      matrix_file,
      check.names = FALSE,
      header = TRUE,
      sep = sep,
      row.names = row.names
    )

  if (any(!rownames(sample_metadata) %in% colnames(matrix_data))) {
    missing <-
      rownames(sample_metadata)[(!rownames(sample_metadata) %in% colnames(matrix_data))]
    stop(
      paste0(
        "Some sample metadata names (",
        paste(missing, collapse = ","),
        ") are absent from the matrix in ",
        matrix_file,
        ", columns are: ",
        paste(colnames(matrix_data), collapse = ",")
      )
    )
  }

  # Allow for feature names appearing in the first column

  if (!is.null(feature_metadata)) {
    if (!any(rownames(feature_metadata) %in% rownames(matrix_data))) {
      rownames(matrix_data) <- matrix_data[, 1]
      if (!any(rownames(feature_metadata) %in% rownames(matrix_data))) {
        stop(paste("All feature metadata names are absent from the matrix in", matrix_file))
      }
    }
  }

  as.matrix(matrix_data[, rownames(sample_metadata), drop = FALSE])
}

#' Read a metadata file
#'
#' @param filename File name
#' @param id_col Identifier column in the file
#' @param sep File separator
#' @param stringsAsFactors Passed to \code{read.delim}
#'
#' @export
#'
#' @return output Data frame
#'
#' @examples
#' metadata_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(
#'     sample = paste0("s", 1:4),
#'     condition = rep(c("treated", "control"), each = 2)
#'   ),
#'   metadata_file, row.names = FALSE
#' )
#' read_metadata(metadata_file, id_col = "sample")
#'
read_metadata <- function(filename, id_col = NULL, sep = NULL, stringsAsFactors = FALSE) {
  if (is.null(sep)) {
    sep <- getSeparator(filename)
  }

  if (!file.exists(filename)) {
    stop(paste("Metadata file", filename, "does not exist."))
  } else {
    metadata <-
      read.delim(
        filename,
        sep = sep,
        check.names = FALSE,
        header = TRUE,
        stringsAsFactors = stringsAsFactors
      )
  }

  if (!is.null(id_col)) {
    if (is.character(id_col) && !id_col %in% colnames(metadata)) {
      stop(
        paste0(
          "Metadata ID column (",
          id_col,
          ") does not exist in metadata ",
          paste(colnames(metadata), collapse = ","),
          " from file ",
          filename
        )
      )
    }

    metadata <- metadata[match(unique(metadata[[id_col]]), metadata[[id_col]]), ]
    rownames(metadata) <- metadata[[id_col]]
  }
  return(metadata)
}
