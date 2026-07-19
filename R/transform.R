#' Apply log2 transformation on a matrix.
#'
#' @param matrix_data A matrix containing data.
#' @param should_transform A boolean indicating if the log2 transformation should be applied.
#'                   If TRUE, log2 transformation is applied unconditionally.
#'                   If FALSE, no transformation is applied.
#'                   If NULL, a conditional transformation based on threshold is applied.
#' @param threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param rmzeros A boolean indicating whether to remove zeros from the matrix.
#'                If TRUE, zeros are removed. Default is FALSE.
#' @param small_value A small value to add to zero entries before log transformation.
#' @param reverse Boolean, should we unlog rather than log?
#'
#' @return A modified matrix.
#' @export
#'
#' @examples
#' # Create a sample matrix
#' mat <- matrix(c(10, 0, 30, 0, 50, 60), ncol = 2)
#'
#' # Use the function with different parameters
#' transformed_mat1 <- cond_log2_transform_matrix(mat, should_transform = TRUE)
#' transformed_mat2 <- cond_log2_transform_matrix(mat, should_transform = NULL, threshold = 40)
#' transformed_mat3 <- cond_log2_transform_matrix(mat, rmzeros = TRUE)
cond_log2_transform_matrix <- function(matrix_data, should_transform = NULL, threshold = 30, rmzeros = FALSE, small_value = 1, reverse = FALSE) {
  # Determine if transformation is needed
  if (is.null(should_transform)) {
    if (reverse) {
      should_transform <- max(matrix_data, na.rm = TRUE) <= threshold
    } else {
      should_transform <- max(matrix_data, na.rm = TRUE) > threshold
    }
  }

  # Apply transformation based on conditions
  if (should_transform) {
    if (reverse) {
      return(2^(matrix_data))
    } else {
      # Handle zeros before any log transform - either removing them or applying a small minimum
      matrix_data[matrix_data == 0] <- if (rmzeros) NA else small_value

      return(log2(matrix_data))
    }
  }

  return(matrix_data)
}

#' Conditionally apply log2 transformation on assay data based on log2_assays parameter.
#'
#' @param assay_data A list containing matrices as assay data.
#' @param log2_assays A string parameter that can be NULL, empty, or a non-empty string.
#'                     If NULL: log2 transformation will be guessed based on input assays.
#'                     If empty: no log2 transformation will be applied.
#'                     If non-empty: log2 transformation will be applied unconditionally to specified assays.
#' @param threshold A numeric threshold to determine if the matrix should be log-transformed.
#'                  This is only checked if should_transform is NULL.
#' @param reverse Boolean, should we unlog rather than log?
#' @param invert_assays Boolean, apply transform to assays NOT specified in log2_assays.
#' @param prettify_names Boolean. Prettify element names? Passed to validate_indices().
#'
#' @return A modified assay_data list.
#' @export
#'
#' @examples
#' assay_data <- list(
#'   counts = matrix(c(1, 100, 1000, 5, 50, 500), nrow = 3,
#'     dimnames = list(paste0("gene", 1:3), c("s1", "s2")))
#' )
#' cond_log2_transform_assays(assay_data, log2_assays = "1")
#'
cond_log2_transform_assays <- function(assay_data, log2_assays, threshold = 30, reverse = FALSE, prettify_names = TRUE, invert_assays = FALSE) {
  indices_to_transform <- c()
  should_transform <- FALSE

  # Check if log2_assays is null
  if (is.null(log2_assays)) {
    indices_to_transform <- names(assay_data)
    should_transform <- NULL
  } else if (log2_assays != "") {
    # Determine which assays to log based on log2_assays
    indices_to_transform <- validate_indices(assay_data = assay_data, index_string = log2_assays, prettify_names = prettify_names, invert_assays = invert_assays)

    should_transform <- TRUE
  }

  # Apply log2 transformation to any specified assays
  for (index in indices_to_transform) {
    assay_data[[index]] <- cond_log2_transform_matrix(matrix_data = assay_data[[index]], should_transform = should_transform, threshold = threshold, reverse = reverse)
  }

  return(assay_data)
}
