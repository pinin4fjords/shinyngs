#' Check whether a list-type slot on an S4 object is populated
#'
#' Centralises the \code{length(x@some_slot) > 0} idiom used in several Shiny
#' modules to decide whether optional data (contrasts, gene sets, read
#' reports, DEXSeq results etc) is available before showing or hiding UI for
#' it.
#'
#' @param x An S4 object (e.g. \code{ExploratorySummarizedExperiment} or
#'   \code{ExploratorySummarizedExperimentList})
#' @param slot_name Name of the slot to check
#'
#' @return output Boolean- is the slot populated?
#'
#' @keywords shiny
#'
#' @examples
#' data(airway, package = "airway")
#' ese <- as(airway, "ExploratorySummarizedExperiment")
#' eselist <- ExploratorySummarizedExperimentList(ese)
#' has_slot_data(eselist, "contrasts")
#'
#' @export

has_slot_data <- function(x, slot_name) {
  length(slot(x, slot_name)) > 0
}

#' Evaluate an expression, converting any error into a Shiny validation message
#'
#' Wraps \code{tryCatch()} around an expression so that an error raised by a
#' computation (e.g. \code{runPCA()} or \code{runClustering()} rejecting
#' degenerate input) surfaces as a \code{\link[shiny]{validate}} message in
#' the UI rather than crashing the app.
#'
#' @param expr Expression to evaluate
#'
#' @return output Result of \code{expr}, or triggers a Shiny validation error
#'   with the original condition's message
#'
#' @keywords shiny
#'
#' @export
#'
#' @examples
#' validateOrCatch(1 + 1)
#'
validateOrCatch <- function(expr) {
  tryCatch(expr, error = function(e) validate(need(FALSE, conditionMessage(e))))
}

#' Call the various read/ validate methods for input data surrounding an experiment
#'
#' @param samples_metadata Sample metadata data frame
#' @param features_metadata Feature metadata data frame
#' @param assay_files List of assay matrices
#' @param contrasts_file Contrasts definition file
#' @param sample_id_col Column of sample metadata used for identifiers
#' @param feature_id_col Column of feature metadata used for identifiers
#' @param assay_names Optional comma-separated list of assay names
#' @param differential_results Optional list of differential stats files
#' @param pval_column P value column if differential stats files specified
#' @param qval_column Q value column if differential stats files specified
#' @param fc_column Fold change column if differential stats files specified
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default, infer and validate from the column name and data
#'   distribution), \code{"log2"} or \code{"linear"}. Each differential
#'   results file is validated independently. See
#'   \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Boolean- should fold changes in stats files be unlogged?
#'
#' @return output A named list with feature/ observation components
#' @export
#'
#' @examples
#' sample_metadata_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(sample = paste0("s", 1:4), condition = rep(c("treated", "control"), each = 2)),
#'   sample_metadata_file, row.names = FALSE
#' )
#' mat <- matrix(1:12, nrow = 3, dimnames = list(paste0("gene", 1:3), paste0("s", 1:4)))
#' matrix_file <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(gene_id = rownames(mat), mat, check.names = FALSE),
#'   matrix_file, row.names = FALSE
#' )
#' validate_inputs(
#'   samples_metadata = sample_metadata_file,
#'   assay_files = matrix_file,
#'   sample_id_col = "sample"
#' )
#'
validate_inputs <- function(samples_metadata,
                            assay_files,
                            contrasts_file = NULL,
                            features_metadata = NULL,
                            sample_id_col = "sample",
                            assay_names = NULL,
                            differential_results = NULL,
                            feature_id_col = "gene_id",
                            pval_column = "pval_column",
                            qval_column = "qval_column",
                            fc_column = "log2FoldChange",
                            fold_change_scale = "auto",
                            unlog_foldchanges = NULL) {
  fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

  validated_parts <- list()

  # Read the sample (observation) - wise metadata

  message(paste(
    "Reading sample sheet at",
    samples_metadata,
    "with ID col",
    sample_id_col
  ))

  samples <- read_metadata(
    filename = samples_metadata,
    id_col = sample_id_col
  )
  validated_parts[[samples_metadata]] <- samples

  # Read feature-wise metadata if provided

  features <- NULL
  if (!is.null(features_metadata)) {
    message(paste(
      "Reading feature metadata at",
      features_metadata,
      "with ID col",
      feature_id_col
    ))

    features <- read_metadata(
      filename = features_metadata,
      id_col = feature_id_col
    )
    validated_parts[[features_metadata]] <- features
  }

  # Read the assay matrices

  assay_files <-
    stringsToNamedVector(
      elements_string = assay_files,
      simplify_files = FALSE,
      prettify_names = FALSE
    )

  # Read the matrices while checking samples and features match columns and rows

  validated_parts[["assays"]] <- lapply(assay_files, function(x) {
    message(paste("Reading assay matrix", x, "and validating against samples and features (if supplied)"))

    mat <- read_matrix(
      matrix_file = x,
      sample_metadata = samples,
      feature_metadata = features
    )
    message(paste("... ", x, "matrix good"))
    mat
  })

  # Read contrasts and check against sample info
  if (!is.null(contrasts_file)) {
    message("Reading contrast definitions and validating against sample sheet")
    validated_parts[[contrasts_file]] <- read_contrasts(contrasts_file, samples)
    message("... contrasts good")
  }

  if (!is.null(differential_results)) {
    contrast_stats_files <-
      stringsToNamedVector(differential_results,
        simplify_files = FALSE,
        prettify_names = FALSE
      )

    validated_parts[["differential_stats"]] <- lapply(contrast_stats_files, function(dsf) {
      read_differential(
        filename = dsf,
        feature_id_column = feature_id_col,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column,
        fold_change_scale = fold_change_scale
      )
    })
  }

  validated_parts
}

#' Validate assay indices based on a given string.
#'
#' This function checks if the provided index string represents valid assays in the given assay data.
#' The function can handle index strings that are comma-separated integers or assay names.
#'
#' @param assay_data A list containing matrices as assay data.
#' @param index_string A string that can be a comma-separated list of integers or assay names.
#' @param invert_assays Boolean, return the indices NOT specified.
#' @param prettify_names Boolean. Prettify element names?
#'
#' @return A vector of valid indices (either as integers or assay names).
#'
#' @examples
#' assay_data_example <- list(a = matrix(1:9, ncol = 3), b = matrix(1:12, ncol = 3), c = matrix(1:6, ncol = 2))
#' valid_assays1 <- validate_indices(assay_data_example, "1,2")
#' valid_assays2 <- validate_indices(assay_data_example, "a,b")
#'
#' @export

validate_indices <- function(assay_data, index_string, invert_assays = FALSE, prettify_names = TRUE) {
  indices_are_names <- TRUE

  if (is_valid_positive_integer_vector(index_string)) {
    indices_are_names <- FALSE
    indices <- as.integer(simpleSplit(index_string))
  } else {
    indices <- simpleSplit(index_string)
    if (prettify_names) {
      indices <- unlist(lapply(indices, prettifyVariablename))
    }
  }

  valid_indices <- c(seq_along(assay_data), names(assay_data))
  invalid_indices <- indices[!indices %in% valid_indices]
  if (length(invalid_indices) > 0) {
    stop(
      "Invalid assays: ",
      paste(invalid_indices, collapse = ", "),
      ", valid indices are:",
      paste(valid_indices, collapse = ", ")
    )
  }

  if (invert_assays) {
    if (indices_are_names) {
      indices <- names(assay_data)[!names(assay_data) %in% indices]
    } else {
      indices <- setdiff(seq_along(assay_data), indices)
    }
  }

  return(indices)
}
