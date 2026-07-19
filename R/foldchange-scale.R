#' Guess whether fold change values are on a log2 or linear scale
#'
#' Shinyngs stores fold changes internally as signed linear values (the ratio
#' of the two conditions, with an absolute magnitude of at least 1 and the
#' sign indicating direction - see \code{\link{fold_change}}), so a value
#' strictly between -1 and 1 is only possible on a log2 scale (e.g. a log2
#' fold change of 0.5 is a ~1.4-fold change). log2 fold changes are also
#' typically small and symmetric around zero, while linear fold changes can
#' have very large magnitudes for the same effect size (e.g. a 1000-fold
#' change is a log2 fold change of ~10).
#'
#' @param values Numeric vector of fold change values (NAs and non-finite
#'   values are ignored).
#' @param log2_magnitude_limit Values with an absolute magnitude greater than
#'   this are considered implausible for a log2 fold change (default 15,
#'   corresponding to a ~32,000-fold change).
#'
#' @return One of \code{"log2"}, \code{"linear"} or \code{"ambiguous"}.
#' @export
#' @examples
#' guess_foldchange_scale(c(-2.1, 0.3, 1.8, -0.05))
#' guess_foldchange_scale(c(-8, 2, 45, -120))
#' guess_foldchange_scale(c(-2, 3, 5, -8))
guess_foldchange_scale <- function(values, log2_magnitude_limit = 15) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return("ambiguous")
  }

  abs_values <- abs(values)

  has_sub_unity <- any(abs_values > 0 & abs_values < 1)
  has_large_magnitude <- any(abs_values > log2_magnitude_limit)

  if (has_sub_unity) {
    return("log2")
  }
  if (has_large_magnitude) {
    return("linear")
  }
  "ambiguous"
}

#' Resolve the scale of a fold-change column, cross-checking a user
#' declaration and a column-naming convention against the observed data
#'
#' Mirrors the log2 guessing done for assay matrices
#' (\code{\link{cond_log2_transform_matrix}}) but for differential statistics
#' fold changes, and errors rather than silently proceeding when the
#' available signals disagree - see
#' \url{https://github.com/pinin4fjords/shinyngs/issues/125}.
#'
#' @param values Numeric vector of fold change values.
#' @param fc_column Name of the column the values came from, used for the
#'   \code{log2FoldChange}-style naming heuristic. Can be \code{NULL} if
#'   unknown.
#' @param declared_scale One of \code{"auto"} (default), \code{"log2"} or
#'   \code{"linear"}.
#'
#' @return Either \code{"log2"} or \code{"linear"}.
#' @export
#' @examples
#' resolve_foldchange_scale(c(-2.1, 0.3, 1.8), fc_column = "log2FoldChange")
#' resolve_foldchange_scale(c(-8, 45, -120), fc_column = "FoldChange")
resolve_foldchange_scale <- function(values, fc_column = NULL, declared_scale = "auto") {
  declared_scale <- match.arg(declared_scale, c("auto", "log2", "linear"))
  if (!is.null(fc_column) && is.na(fc_column)) {
    fc_column <- NULL
  }

  column_label <- if (is.null(fc_column)) "the fold change column" else paste0("'", fc_column, "'")
  name_hint <- if (!is.null(fc_column) && grepl("log2", fc_column, ignore.case = TRUE)) "log2" else NULL
  distribution_guess <- guess_foldchange_scale(values)

  contradicts <- function(a, b) !is.null(a) && !is.null(b) && b != "ambiguous" && a != b

  if (declared_scale != "auto") {
    if (contradicts(declared_scale, distribution_guess)) {
      stop(paste0(
        "--fold_change_scale/fold_change_scale was set to '", declared_scale, "', but the values in ",
        column_label, " look like ", distribution_guess, " fold changes (values with an absolute ",
        "magnitude below 1 imply log2; very large excursions imply linear). Check fold_change_scale ",
        "and fc_column, or pass the correct scale explicitly."
      ))
    }
    if (contradicts(declared_scale, name_hint)) {
      stop(paste0(
        "fold_change_scale was set to '", declared_scale, "', but the column name ", column_label,
        " conventionally holds log2 fold changes. Check fold_change_scale and fc_column."
      ))
    }
    return(declared_scale)
  }

  if (contradicts(name_hint, distribution_guess)) {
    stop(paste0(
      "Column name ", column_label, " suggests log2 fold changes, but the observed values look like ",
      distribution_guess, " fold changes. Set fold_change_scale explicitly ('log2' or 'linear') to resolve this."
    ))
  }

  if (distribution_guess != "ambiguous") {
    return(distribution_guess)
  }
  if (!is.null(name_hint)) {
    return(name_hint)
  }

  stop(paste0(
    "Could not determine whether the fold change values in ", column_label, " are log2 or linear: ",
    "the column name gives no hint and the distribution is ambiguous (no values below 1 in magnitude, ",
    "none very large). Set fold_change_scale explicitly ('log2' or 'linear') to resolve this."
  ))
}

#' Map the deprecated \code{unlog_foldchanges}/\code{--unlog_foldchanges}
#' argument onto \code{fold_change_scale}, warning if it was used
#'
#' Shared by \code{read_differential}, \code{compile_contrast_data},
#' \code{validate_inputs} and the \code{make_app_from_files.R},
#' \code{differential_plots.R} and \code{validate_fom_components.R} scripts
#' under \code{exec/}, so the shim only needs to be written once.
#'
#' @param fold_change_scale The caller's current \code{fold_change_scale}
#'   value (used unchanged when \code{unlog_foldchanges} is \code{NULL}).
#' @param unlog_foldchanges The deprecated argument value, or \code{NULL} if
#'   it was not supplied.
#'
#' @return The \code{fold_change_scale} to use.
#' @export
#'
#' @examples
#' resolve_deprecated_unlog_foldchanges("auto", NULL)
#' resolve_deprecated_unlog_foldchanges("auto", TRUE)
#'
resolve_deprecated_unlog_foldchanges <- function(fold_change_scale, unlog_foldchanges) {
  if (is.null(unlog_foldchanges)) {
    return(fold_change_scale)
  }

  warning(
    "`unlog_foldchanges`/`--unlog_foldchanges` is deprecated and will be removed in a future release; ",
    "use `fold_change_scale`/`--fold_change_scale` = \"log2\" (to unlog) or \"linear\" (to leave values ",
    "as-is) instead.",
    call. = FALSE
  )
  if (isTRUE(unlog_foldchanges)) "log2" else "linear"
}

# Unlog a signed fold change value from log2 to linear scale (see
# resolve_foldchange_scale()).
unlog_fold_change <- function(x) {
  sign(x) * 2^(abs(x))
}

# Read the feature id/p value/q value/fold change columns of a differential
# statistics file, without resolving or applying any fold change scale.
# Shared by read_differential() (which resolves/applies the scale for a
# single file) and compile_contrast_data() (which combines several files'
# raw fold changes before resolving the scale once across all of them).
read_stats_table <- function(filename, feature_id_column = NULL, pval_column = NULL, qval_column = NULL, fc_column = NULL) {
  st <- read_metadata(filename, id_col = feature_id_column)
  stats_cols <- c(feature_id_column, pval_column, qval_column, fc_column)

  success <-
    check_list_is_subset(
      stats_cols,
      colnames(st),
      "stats variables",
      "available stats columns"
    )
  st <- st[, stats_cols]

  for (c in colnames(st)) {
    st[[c]][grep("^ *NA$", st[[c]])] <- NA
    if (c %in% c(pval_column, qval_column, fc_column)) {
      st[[c]] <- as.numeric(st[[c]])
    }
  }
  st
}

#' Read tables of differential statistics
#'
#' @param filename File name of file with table of differential statistics
#' @param feature_id_column Column of stats file with feature identifiers
#' @param pval_column Column of stats file with p values
#' @param qval_column Column of stats file with adjust p values/ q values
#' @param fc_column Column of stats with fold changes
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default, infer and validate from the column name and data
#'   distribution), \code{"log2"} or \code{"linear"}. See
#'   \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Reverse a log on fold changes? Set to TRUE if values are logged.
#'
#' @return output Validated selected columns of differential stats files as a
#'   data frame, with the resolved scale attached as the \code{fold_change_scale}
#'   attribute.
#' @export
#'
#' @examples
#' stats_file <- tempfile(fileext = ".tsv")
#' write.table(
#'   data.frame(
#'     gene_id = paste0("gene", 1:5),
#'     pvalue = c(0.001, 0.2, 0.03, 0.5, 0.008),
#'     padj = c(0.01, 0.4, 0.1, 0.7, 0.04),
#'     log2FoldChange = c(2.5, -0.1, 1.2, 0.3, -3.1)
#'   ),
#'   stats_file, sep = "\t", row.names = FALSE, quote = FALSE
#' )
#' read_differential(stats_file,
#'   feature_id_column = "gene_id",
#'   pval_column = "pvalue",
#'   qval_column = "padj",
#'   fc_column = "log2FoldChange"
#' )
#'
read_differential <- function(filename,
                              feature_id_column = NULL,
                              pval_column = NULL,
                              qval_column = NULL,
                              fc_column = NULL,
                              fold_change_scale = "auto",
                              unlog_foldchanges = NULL) {
  fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

  st <- read_stats_table(
    filename = filename,
    feature_id_column = feature_id_column,
    pval_column = pval_column,
    qval_column = qval_column,
    fc_column = fc_column
  )

  resolved_scale <- resolve_foldchange_scale(
    values = st[[fc_column]],
    fc_column = fc_column,
    declared_scale = fold_change_scale
  )

  if (resolved_scale == "log2") {
    st[[fc_column]] <- unlog_fold_change(st[[fc_column]])
  }

  attr(st, "fold_change_scale") <- resolved_scale
  st
}

#' Compile contrast stats for inclusion in shinyngs
#'
#' @param differential_stats_files Tabular files with differential stats
#' @param pval_column P value column in stats files
#' @param qval_column Q value column in stats files
#' @param fc_column Fold change column in stats files
#' @param feature_id_column Feature identifier column in stats files
#' @param fold_change_scale Scale of the values in \code{fc_column}: one of
#'   \code{"auto"} (default), \code{"log2"} or \code{"linear"}. Resolved once
#'   across the fold changes combined from all \code{differential_stats_files},
#'   rather than per-file, so contrasts from the same experiment are treated
#'   consistently. See \code{\link{resolve_foldchange_scale}}.
#' @param unlog_foldchanges Deprecated, use \code{fold_change_scale} instead.
#'   Should fold change values be unlogged?
#'
#' @return output A named list of data frames by statistic, number of columns equal to input file number

compile_contrast_data <-
  function(differential_stats_files,
           feature_id_column = NULL,
           pval_column = NULL,
           qval_column = NULL,
           fc_column = NULL,
           fold_change_scale = "auto",
           unlog_foldchanges = NULL) {
    fold_change_scale <- resolve_deprecated_unlog_foldchanges(fold_change_scale, unlog_foldchanges)

    # Read stats and make sure they're numeric. Fold changes are read raw here
    # (no scale applied) so that the scale can be resolved once below, across
    # the fold changes combined from every contrast file.

    contrast_stats <- lapply(differential_stats_files, function(dsf) {
      read_stats_table(
        filename = dsf,
        feature_id_column = feature_id_column,
        pval_column = pval_column,
        qval_column = qval_column,
        fc_column = fc_column
      )
    })

    contrast_stats_rearranged <- list()

    add_to_stats <- function(source) {
      df <- do.call(cbind, lapply(contrast_stats, function(x) {
        x[, source, drop = FALSE]
      }))
      names(df) <- paste0("V", seq_len(ncol(df)))
      rownames(df) <- contrast_stats[[1]][[feature_id_column]]
      df
    }

    if (!is.null(fc_column)) {
      fold_changes <- add_to_stats(source = fc_column)

      resolved_scale <- resolve_foldchange_scale(
        values = unlist(fold_changes),
        fc_column = fc_column,
        declared_scale = fold_change_scale
      )

      if (resolved_scale == "log2") {
        fold_changes[] <- lapply(fold_changes, unlog_fold_change)
      }

      attr(fold_changes, "fold_change_scale") <- resolved_scale
      contrast_stats_rearranged[["fold_changes"]] <- fold_changes
    }
    if (!is.null(pval_column)) {
      contrast_stats_rearranged[["pvals"]] <- add_to_stats(source = pval_column)
    }
    if (!is.null(qval_column)) {
      contrast_stats_rearranged[["qvals"]] <- add_to_stats(source = qval_column)
    }
    contrast_stats_rearranged
  }
