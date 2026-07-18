#' Check one list is a subset of another and throw an error if not
#'
#' @param test_list  Test list
#' @param reference_list Reference list
#' @param test_list_name Name of test list for error
#' @param reference_list_name Name of reference list for error
#'
#' @return output Returns TRUE if check passes
#' @export

checkListIsSubset <- function(test_list,
                              reference_list,
                              test_list_name,
                              reference_list_name) {
  if (!all(test_list %in% reference_list)) {
    missing_vars <- test_list[!test_list %in% reference_list]
    stop(
      paste0(
        "Not all ",
        test_list_name,
        " are available in the ",
        reference_list_name,
        ".\n",
        "Missing ", test_list_name, ": ", paste(missing_vars, collapse = ", "), "\n",
        "Available ", reference_list_name, ": ", paste(unique(reference_list), collapse = ", ")
      )
    )
  }
  TRUE
}

#' Remove random effects from a model formula
#'
#' @param formula_string Formula string that may contain random effects
#'
#' @return output Fixed-effects-only formula
fixedEffectsFormula <- function(formula_string) {
  reformulas::nobars(as.formula(formula_string))
}

#' Build a model matrix from the fixed-effects part of a formula
#'
#' @param formula_string Formula string that may contain random effects
#' @param samples Data frame of sample information
#'
#' @return output Model matrix for the fixed-effects formula
fixedEffectsModelMatrix <- function(formula_string, samples) {
  model.matrix(fixedEffectsFormula(formula_string), data = samples)
}

#' Validate a formula-based contrast string against fixed-effect coefficients
#'
#' @param contrast_id Contrast identifier
#' @param contrast_formula Formula string used for the contrast
#' @param contrast_string Contrast string to validate
#' @param model_matrix Optional precomputed model matrix for the fixed-effects formula
#' @param samples Data frame of sample information
#'
#' @return output Returns TRUE if validation passes
validateFormulaBasedContrast <- function(contrast_id,
                                         contrast_formula,
                                         contrast_string,
                                         model_matrix = NULL,
                                         samples) {
  if (is.null(model_matrix)) {
    model_matrix <- fixedEffectsModelMatrix(contrast_formula, samples)
  }
  model_coefficients <- make.names(colnames(model_matrix), unique = TRUE)

  tryCatch(
    limma::makeContrasts(contrasts = contrast_string, levels = model_coefficients),
    error = function(e) {
      stop(
        paste0(
          "Contrast id '", contrast_id, "' has invalid make_contrasts_str '", contrast_string,
          "' for formula '", contrast_formula, "'. ",
          "Available coefficient names for make_contrasts_str: ",
          paste(model_coefficients, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  )
}


#' Read and validate a contrasts file against sample metadata
#'
#' Checks:
#' 1. No duplicate contrast IDs. Ensure that the required columns (variable, reference, target) are present.
#' 2. Values in the contrast variable column exist as column names in the sample metadata.
#' 3. If blocking factors are supplied, checks that they are present in the sample metadata.
#' 4. Design matrix is full rank.
#' 5. Warn about continuous covariates (e.g. numeric patient IDs treated as continuous).
#' 6. Values of specified columns don't contain special characters.
#' 7. Verify that the specified reference and target values exist in the corresponding sample metadata column.
#' 8. Issue a warning if the reference and target levels are identical.
#'
#' @param filename Contrasts file
#' @param samples Data frame of sample information
#' @param variable_column Column in contrasts file referencing sample sheet
#'   column
#' @param reference_column Column in contrast file referencing reference level
#'   of sample sheet variable
#' @param target_column Column in contrast file referencing target level of
#'   sample sheet variable
#' @param blocking_column Colon-separated column in contrast file referencing
#'   sample sheet variables to be used as blocking factors
#' @param convert_to_list Convert output to a list as used internally by
#'   shinyngs?
#' @param validate_design Validate design matrix (check for NAs, full rank,
#'   numeric columns, special characters)? Set to FALSE to skip these checks.
#'
#' @return output Validated contrasts data frame
#' @export

read_contrasts <-
  function(filename,
           samples,
           variable_column = "variable",
           reference_column = "reference",
           target_column = "target",
           blocking_column = "blocking",
           convert_to_list = FALSE,
           validate_design = TRUE) {
    # Read the contrasts depending on the file format (CSV or YAML)
    if (grepl("\\.csv$", filename)) {
      contrasts <- read_metadata(filename)
      # Check for duplicates in constrasts id
      if (any(duplicated(contrasts$id))) {
        stop("Duplicate contrast ids found in CSV contrasts file.")
      }
      contrast_cols <- c(variable_column, reference_column, target_column)
      if (!blocking_column %in% names(contrasts)) {
        contrasts[[blocking_column]] <- NA
      }

      # Check contrast headers are as expected
      if (!all(contrast_cols %in% colnames(contrasts))) {
        stop(paste("Contrasts file must contain all of", paste(contrast_cols, collapse = ", ")))
      }
    } else if (grepl("\\.ya?ml$", filename)) {
      contrasts_yaml <- yaml::read_yaml(filename)

      if (is.null(contrasts_yaml$contrasts)) {
        stop("YAML file must contain a 'contrasts' section.")
      }

      # Parse YAML contrasts into a data frame
      contrasts <- do.call(rbind, lapply(contrasts_yaml$contrasts, function(x) {
        stopifnot(!is.null(x$id)) # Require that each contrast has an 'id'

        row <- data.frame(
          id = x$id,
          variable = NA, reference = NA, target = NA,
          blocking = NA,
          exclude_samples_col = NA,
          exclude_samples_values = NA,
          formula = NA,
          make_contrasts_str = NA
        )

        # A contrast can be defined in two ways:
        #
        # 1 . Specifying a column in the sample sheet, and two values from that column which define two groups of samples to compare. Tools using a contrast defined in this way would need to generate a model and contrast using that information.
        # 2. Specifying the formula and contrast string explicitly. Tools using those contrasts can then pass these directly to function of suites such as DESeq2.

        if (!is.null(x$comparison)) {
          if (!is.null(x$formula) || !is.null(x$make_contrasts_str)) {
            stop(sprintf("Contrast id '%s' with 'comparison' must not have 'formula' or 'make_contrasts_str'.", x$id))
          }

          fields <- setNames(rep(NA, 3), c("variable", "reference", "target"))
          fields[names(fields)[seq_along(x$comparison)]] <- x$comparison

          if (any(is.na(fields)) || any(fields == "")) {
            stop(sprintf("Contrast id '%s' must provide non-empty 'variable', 'reference', and 'target' fields.", x$id))
          }

          row$variable <- fields["variable"]
          row$reference <- fields["reference"]
          row$target <- fields["target"]

          if (!is.null(x$blocking_factors)) {
            row$blocking <- paste(x$blocking_factors, collapse = ";")
          }
        } else if (!is.null(x$formula)) {
          if (is.null(x$make_contrasts_str) || !is.null(x$blocking_factors)) {
            stop(sprintf("Contrast id '%s' with 'formula' must have 'make_contrasts_str' and no 'blocking_factors'.", x$id))
          }

          row$formula <- x$formula
          row$make_contrasts_str <- x$make_contrasts_str
        } else {
          stop(sprintf("Contrast id '%s' must provide either 'comparison' or 'formula' + 'make_contrasts_str'.", x$id))
        }

        if (!is.null(x$exclude_samples_col) || !is.null(x$exclude_samples_values)) {
          if (is.null(x$exclude_samples_col) || is.null(x$exclude_samples_values)) {
            stop(sprintf("Contrast id '%s' must provide both 'exclude_samples_col' and 'exclude_samples_values'.", x$id))
          }

          row$exclude_samples_col <- x$exclude_samples_col
          row$exclude_samples_values <- paste(x$exclude_samples_values, collapse = ";")
        }

        row
      }))
      if (any(duplicated(contrasts$id))) {
        stop("Duplicate contrast ids found in YAML contrasts file.")
      }
    } else {
      stop("Invalid file format. Please provide a CSV or YAML file.")
    }

    # Check contrast content is appropriate to sample sheet
    variables_without_na <- na.omit(contrasts$variable)
    if (length(variables_without_na) > 0) {
      success <- checkListIsSubset(variables_without_na, colnames(samples), "contrast variables", "sample metadata")
    }
    # Check blocking variables, where supplied
    blocking <- unlist(lapply(contrasts[[blocking_column]], function(x) simpleSplit(x, ";")))
    blocking <- blocking[!is.na(blocking)]
    if (length(blocking) > 0) {
      success <- checkListIsSubset(blocking, colnames(samples), "blocking variables", "sample metadata")
    }
    if ("exclude_samples_col" %in% colnames(contrasts)) {
      exclude_cols <- na.omit(contrasts$exclude_samples_col)
      if (length(exclude_cols) > 0) {
        success <- checkListIsSubset(exclude_cols, colnames(samples), "exclude sample columns", "sample metadata")
      }
    }

    # Ensure reference and target are valid for their variable
    for (i in seq_len(nrow(contrasts))) {
      blocking_vars <- simpleSplit(contrasts[[blocking_column]][i], ";")
      design_cols <- character(0)

      # Extract design matrix columns from contrasts: the variable column plus any blocking factors.
      # For formula-based contrasts, extract variables from the formula itself.
      if (validate_design) {
        # Filter samples if exclude columns are specified for this contrast
        contrast_samples <- samples
        if ("exclude_samples_col" %in% colnames(contrasts) && "exclude_samples_values" %in% colnames(contrasts)) {
          if (!is.na(contrasts$exclude_samples_col[i]) && !is.na(contrasts$exclude_samples_values[i])) {
            exclude_col <- contrasts$exclude_samples_col[i]
            exclude_vals <- simpleSplit(contrasts$exclude_samples_values[i], ";")
            contrast_samples <- samples[!samples[[exclude_col]] %in% exclude_vals, , drop = FALSE]
          }
        }

        if ("formula" %in% colnames(contrasts) && !is.na(contrasts$formula[i])) {
          design_cols <- unique(all.vars(as.formula(contrasts$formula[i])))
          success <- checkListIsSubset(design_cols, colnames(samples), "formula variables", "sample metadata")
          mm <- fixedEffectsModelMatrix(contrasts$formula[i], contrast_samples)

          validateFormulaBasedContrast(
            contrast_id = contrasts[i, "id"],
            contrast_formula = contrasts$formula[i],
            contrast_string = contrasts$make_contrasts_str[i],
            model_matrix = mm,
            samples = contrast_samples
          )
        } else {
          design_cols <- unique(na.omit(c(contrasts[[variable_column]][i], blocking_vars)))
          mm <- model.matrix(~ . - 1, data = contrast_samples[, design_cols, drop = FALSE])
        }

        design_matrix <- contrast_samples[, design_cols, drop = FALSE]

        # Ensure there are no NA values in the design matrix.
        if (any(is.na(design_matrix))) {
          stop("NA values found in one or more design matrix columns.")
        }

        # Check that the design matrix is full rank.
        if (qr(mm)$rank < ncol(mm)) {
          stop(paste("Design matrix is not full rank.", "Model matrix columns:", paste(colnames(mm), collapse = ", "), "\n"))
        }

        # Warn about continuous covariates in the design matrix columns.
        for (col in design_cols) {
          if (is.numeric(samples[[col]])) {
            warning(paste("Column", col, "is numeric and may be treated as continuous."))
          }
        }

        # Check that values in design matrix columns do not contain disallowed special characters.
        for (col in design_cols) {
          vals <- as.character(samples[[col]])
          for (sc in c("/", "\\\\")) { # Default special characters: c("/", "\\\\")
            if (any(grepl(sc, vals))) {
              warning(paste(
                "Column", col, "contains special character", sc,
                "which may cause issues downstream."
              ))
            }
          }
        }
      }

      var <- contrasts[i, variable_column]
      ref <- contrasts[i, reference_column]
      tgt <- contrasts[i, target_column]

      # Only check values if both reference and target are present
      if (!is.na(ref) && !is.na(tgt)) {
        success <- checkListIsSubset(ref, samples[[var]], "contrast levels", "sample metadata variable")
        success <- checkListIsSubset(tgt, samples[[var]], "contrast levels", "sample metadata variable")

        if (ref == tgt) {
          warning(sprintf("Contrast id '%s' has identical reference and target levels.", contrasts[i, "id"]))
        }
      }
    }

    # Convert contrasts to a list if requested
    if (convert_to_list) {
      contrasts <- apply(contrasts, 1, function(x) {
        conlist <- split(unname(x), names(x))[names(x)]
        rename <- c("variable" = "Variable", "reference" = "Group.1", "target" = "Group.2")
        rename_ind <- match(names(rename), names(conlist))
        names(conlist)[rename_ind] <- rename
        nonempty <- unlist(lapply(conlist, function(y) !(is.na(y) || is.null(y) || grepl("^\\s*$", y))))
        conlist[nonempty]
      })
    }

    contrasts
  }
