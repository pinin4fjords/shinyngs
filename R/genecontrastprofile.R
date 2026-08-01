#' Plot one feature's fold change across contrasts
#'
#' Builds an interactive dot plot from a gene-page contrast table. Fold changes
#' use shinyngs' signed linear representation and are shown on a signed log2
#' scale. When q values are present, filled markers meet \code{q_threshold},
#' open markers do not, and crosses have no q value.
#'
#' @param contrast_table Data frame containing a \code{Fold change} column
#'   and either a \code{Contrast} column or \code{Variable},
#'   \code{Condition 1}, and \code{Condition 2} columns.
#' @param q_threshold Numeric q-value threshold used to distinguish marker
#'   symbols.
#' @param title Plot title.
#'
#' @return A plotly object.
#' @export
#'
#' @examples
#' interactive_gene_contrast_profile(data.frame(
#'   Variable = c("condition", "condition"),
#'   `Condition 1` = c("control", "control"),
#'   `Condition 2` = c("treated_a", "treated_b"),
#'   `Fold change` = c(2, -4),
#'   `q value` = c(0.01, 0.2),
#'   check.names = FALSE
#' ))
interactive_gene_contrast_profile <- function(contrast_table, q_threshold = 0.05, title = "Fold change by contrast") {
  if (!"Fold change" %in% colnames(contrast_table)) {
    stop("interactive_gene_contrast_profile(): missing required column: Fold change")
  }
  contrast_columns <- c("Variable", "Condition 1", "Condition 2")
  if (!"Contrast" %in% colnames(contrast_table) && !all(contrast_columns %in% colnames(contrast_table))) {
    stop("interactive_gene_contrast_profile(): supply Contrast or Variable, Condition 1, and Condition 2 columns")
  }
  if (!is.numeric(q_threshold) || length(q_threshold) != 1 || is.na(q_threshold) || q_threshold < 0 || q_threshold > 1) {
    stop("interactive_gene_contrast_profile(): 'q_threshold' must be one number between 0 and 1")
  }

  fold_change <- suppressWarnings(as.numeric(contrast_table[["Fold change"]]))
  log2_fold_change <- log_fold_change(fold_change)
  keep <- is.finite(log2_fold_change)
  if (!any(keep)) {
    stop("interactive_gene_contrast_profile(): no finite fold changes to plot")
  }

  profile <- contrast_table[keep, , drop = FALSE]
  profile$fold_change <- fold_change[keep]
  profile$log2_fold_change <- log2_fold_change[keep]
  profile$contrast <- if ("Contrast" %in% colnames(profile)) {
    as.character(profile$Contrast)
  } else {
    paste0(
      prettify_variable_name(as.character(profile$Variable)), ": ",
      profile[["Condition 2"]], " vs ", profile[["Condition 1"]]
    )
  }
  profile <- profile[order(-abs(profile$log2_fold_change), profile$contrast, method = "radix"), , drop = FALSE]

  has_q_values <- "q value" %in% colnames(profile)
  if (has_q_values) {
    q_values <- suppressWarnings(as.numeric(profile[["q value"]]))
    profile$significance <- ifelse(!is.finite(q_values), "q value unavailable", ifelse(q_values <= q_threshold, paste0("q \u2264 ", q_threshold), paste0("q > ", q_threshold)))
    profile$marker_symbol <- ifelse(!is.finite(q_values), "x", ifelse(q_values <= q_threshold, "circle", "circle-open"))
  } else {
    profile$significance <- "q value unavailable"
    profile$marker_symbol <- "x"
  }

  hover <- paste0(
    "<b>", htmltools::htmlEscape(profile$contrast), "</b>",
    "<br>Fold change: ", signif(profile$fold_change, 4),
    "<br>log2 fold change: ", signif(profile$log2_fold_change, 4)
  )
  if ("p value" %in% colnames(profile)) {
    hover <- paste0(hover, "<br>p value: ", signif(suppressWarnings(as.numeric(profile[["p value"]])), 4))
  }
  if (has_q_values) {
    hover <- paste0(hover, "<br>q value: ", signif(suppressWarnings(as.numeric(profile[["q value"]])), 4))
  }
  hover <- paste0(hover, "<br>", profile$significance)

  p <- plot_ly() %>%
    add_markers(
      x = profile$log2_fold_change, y = profile$contrast,
      text = hover, hovertemplate = "%{text}<extra></extra>",
      marker = list(
        color = SHINYNGS_ACCENT, size = 12,
        symbol = profile$marker_symbol,
        line = list(width = 2, color = SHINYNGS_ACCENT)
      ),
      showlegend = FALSE
    )

  subtitle <- if (has_q_values) {
    paste0("<br><sup>Ordered by absolute effect \u00b7 Filled: q \u2264 ", q_threshold, " \u00b7 Open: q > ", q_threshold, " \u00b7 \u00d7: q unavailable</sup>")
  } else {
    "<br><sup>Ordered by absolute effect \u00b7 \u00d7: q unavailable</sup>"
  }

  layout(
    p,
    title = paste0(htmltools::htmlEscape(title), subtitle),
    xaxis = list(title = "log2 fold change", zeroline = FALSE),
    yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = rev(profile$contrast)),
    shapes = list(list(type = "line", x0 = 0, x1 = 0, y0 = 0, y1 = 1, yref = "paper", line = list(color = "#8A8F98", width = 1, dash = "dot"))),
    margin = list(l = 40, r = 20, b = 60, t = 80)
  )
}
