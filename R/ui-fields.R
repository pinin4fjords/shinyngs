#' Append a help icon carrying a tooltip to a label
#'
#' Used to attach non-obvious control-level guidance at the point of use,
#' rather than requiring a user to open a panel-level help modal to
#' understand an individual field.
#'
#' @param label Label text or tag content
#' @param tooltip Tooltip text shown on hover/focus of the icon. If \code{NULL},
#'   \code{label} is returned unchanged and no icon is added.
#' @param placement Tooltip placement, passed to \code{\link[bslib]{tooltip}}
#'
#' @return A \code{tagList} combining the label and, if \code{tooltip} is
#'   supplied, a tooltip-triggering help icon.
#' @export
#'
#' @examples
#' with_help_icon("Whisker distance", "How far outliers may sit from the box")
#'
with_help_icon <- function(label, tooltip = NULL, placement = "right") {
  if (is.null(tooltip)) {
    return(label)
  }

  tagList(label, " ", bslib::tooltip(
    tags$span(icon("circle-info", verify_fa = FALSE), style = "cursor: help; color: var(--bs-secondary-color);"),
    tooltip,
    placement = placement
  ))
}

#' Give a terse control an accessible name and a hover/focus tooltip
#'
#' Icon-only or single-word controls (the dark-mode toggle, plot/table
#' download buttons, help-modal triggers) carry little visible text, so they
#' are hard to identify by mouse, keyboard or screen reader. This sets an
#' \code{aria-label} for assistive technology and wraps the control in a short
#' \code{bslib::tooltip()} that surfaces on both hover and keyboard focus.
#'
#' @param tag A single UI element to annotate
#' @param label Accessible name, applied as the tag's \code{aria-label}
#' @param tooltip Tooltip text shown on hover/focus; defaults to \code{label}
#' @param placement Tooltip placement, passed to \code{\link[bslib]{tooltip}}
#'
#' @return \code{tag} wrapped in a \code{bslib::tooltip()}
#'
#' @keywords internal
#'
a11yControl <- function(tag, label, tooltip = label, placement = "top") {
  tag <- tagAppendAttributes(tag, `aria-label` = label)
  bslib::tooltip(tag, tooltip, placement = placement)
}

#' Wrap a Shiny input so its label is displayed inline
#'
#' @param field_def A field definition with NULL set for the label property
#' @param label Field label
#' @param labelwidth With (in units out of 12) for label
#' @param tooltip Optional tooltip text explaining the field, shown via a help
#'   icon next to the label (see \code{\link{with_help_icon}})
#'
#' @return output A UI definition that can be passed to the shinyUI function.
#' @export
#'
#' @examples
#' inline_field(numericInput("foo", label = NULL, min = 0, max = 100, value = 50), "FOO")
#'
inline_field <- function(field_def, label, labelwidth = 6, tooltip = NULL) {
  label_content <- with_help_icon(HTML(paste0("<b>", label, ":</b>&nbsp;")), tooltip)
  fluidRow(column(labelwidth, label_content), column(12 - labelwidth, field_def))
}



#' Make a numeric field with selectable associated cardinality (>, < ..).
#'
#' A wrapper around \code{\link[shiny]{numericInput}}, providing an inline
#' label and an associated field specifying how the value should be applied,
#' i.e. 'greater than this value' etc.
#'
#' @param id ID to use for the numeric field
#' @param cardinal_id ID to use for the cardinality field
#' @param label Label
#' @param value Default value
#' @param cardinality Default cardinality
#' @param step Passed to \code{\link[shiny]{numericInput}}
#' @param min Passed to \code{\link[shiny]{numericInput}}
#' @param max Passed to \code{\link[shiny]{numericInput}}
#' @param tooltip Optional tooltip text explaining the field, shown via a help
#'   icon next to the label (see \code{\link{with_help_icon}})
#'
#' @return out An HTML tag object that can be rendered as HTML using
#' as.character()

cardinalNumericField <- function(id, cardinal_id, label, value, cardinality = "<=", step = NA, min = NA, max = NA, tooltip = NULL) {
  label_content <- with_help_icon(HTML(paste0("<b>", label, ":</b>&nbsp;")), tooltip)
  tags$div(
    fluidRow(column(4, label_content), column(3, selectInput(cardinal_id, label = NULL, choices = c(
      "<=", ">=", ">= or <= -",
      "<= and >= -"
    ), selected = cardinality), selectize = FALSE), column(5, numericInput(id, label = NULL, value = value, min = min, max = max, step = step))),
    class = "shinyngs-cardinalfield"
  )
}

#' Evaluate a vector of values with respect to a limit and a cardinality, being
#' '>', '<' , '> or <-' (e.g. a fold change above a limit in + or -
#' directions), or '< and >-' (not a above a limit in + or -).
#'
#' @param values Vector of numeric values
#' @param cardinality Cardinality: '>', '<' , '> or <-'
#' @param limit Numeric limit
#'
#' @return out A logical vector

evaluateCardinalFilter <- function(values, cardinality, limit) {
  if (cardinality == "<=") {
    (!is.na(values)) & values <= limit
  } else if (cardinality == ">=") {
    (!is.na(values)) & values >= limit
  } else if (cardinality == ">= or <= -") {
    (!is.na(values)) & abs(values) >= limit
  } else if (cardinality == "<= and >= -") {
    (!is.na(values)) & values <= limit & values >= -limit
  } else {
    stop("invalid cardinality")
  }
}
