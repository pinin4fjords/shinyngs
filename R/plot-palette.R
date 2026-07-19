#' Resolve the default grouping variable for an experiment list
#'
#' Returns \code{default_groupvar} if set, otherwise the first of
#' \code{group_vars}, or \code{NULL} when no grouping variables are defined.
#'
#' @param eselist ExploratorySummarizedExperimentList object
#'
#' @return A single grouping-variable name, or \code{NULL}
#'
#' @keywords internal
#'
defaultGroupvar <- function(eselist) {
  if (has_slot_data(eselist, "default_groupvar")) {
    eselist@default_groupvar
  } else if (has_slot_data(eselist, "group_vars")) {
    eselist@group_vars[1]
  } else {
    NULL
  }
}

#' Choose a valid set of grouping variables from a targets/ experiment data
#' frame.
#'
#' To be useful for grouping a variable needs to be a character or a factor,
#' and to have a number of values greater than one but less than the number of
#' samples.
#'
#' @param df Input data frame
#'
#' @return out A list of valid column names
#' @export
#'
#' @examples
#' # `airway` contains info on the samples it's based on
#'
#' require(airway)
#' data(airway, package = "airway")
#'
#' # However, not all variables are useful for grouping data. Some have a
#' # different value for every sample, one has the same value for all of them.
#'
#' colData(airway)
#'
#' # So just pick the variables that ARE useful
#'
#' choose_grouping_variables(data.frame(colData(airway)))
#'
choose_grouping_variables <- function(df) {
  all_vars <- colnames(df)
  vartypes <- unlist(lapply(df, class))
  numunique <- unlist(lapply(df, function(x) length(unique(x[!is.na(x)]))))

  all_vars[vartypes != "integer" & numunique < nrow(df) & numunique > 1]
}

#' Group levels for a colouring variable, in first-appearance order
#'
#' Normalises the values of a colouring column into the set of group levels used
#' to build coloured plotly traces. NA becomes "N/A"; an absent colouring
#' variable yields no groups. Shared by the plot builders and their Shiny
#' modules so trace order and the server's trace-index-to-group mapping cannot
#' drift apart.
#'
#' @param experiment Sample annotation data frame
#' @param colorby Column name in \code{experiment} used for grouping, or NULL
#'
#' @return A character vector of group levels
#'
#' @noRd
groupLevels <- function(experiment, colorby = NULL) {
  if (is.null(colorby)) {
    return(character(0))
  }
  unique(na_replace(as.character(experiment[[colorby]]), "N/A"))
}

#' Resolve a colour palette to a vector named by group level
#'
#' Regenerates the palette when none was supplied or it is too short to cover
#' the levels, then names it by level so traces can look up their colour by
#' group.
#'
#' @param palette Palette of colours, or NULL to generate one
#' @param levels Character vector of group levels to colour
#' @param palette_name Valid R colour palette name, used when generating
#'
#' @return A character vector of colours named by \code{levels}
#'
#' @noRd
resolvePalette <- function(palette, levels, palette_name = COLORBLIND_PALETTE_NAME) {
  if (is.null(palette) || any(is.na(palette[seq_along(levels)]))) {
    palette <- make_color_scale(max(length(levels), 1), palette = palette_name)
  }
  stats::setNames(palette[seq_along(levels)], levels)
}

#' Track groups toggled off via a plotly legend
#'
#' Wires up the reactive plumbing shared by plot modules whose legend entries
#' correspond to groups: clicking an entry drops that group so the plot is
#' redrawn on the remainder. Sets up a reactive value of hidden groups, resets
#' it when the grouping variable changes, and translates \code{plotly_restyle}
#' events into additions/removals.
#'
#' @param plot_source The plotly source id the plot was built with
#' @param getLevels Reactive returning the group levels, in the order legend
#'   traces are built
#' @param resetOn Reactive whose change clears the hidden set (typically the
#'   grouping variable)
#' @param trace_offset Number of leading non-group traces before the per-group
#'   traces, so a restyled trace index maps onto \code{getLevels()}
#'
#' @return A reactive value holding the character vector of hidden groups
#'
#' @noRd
legendHiddenGroups <- function(plot_source, getLevels, resetOn, trace_offset = 0L) {
  hiddenGroups <- reactiveVal(character(0))

  observeEvent(resetOn(),
    {
      hiddenGroups(character(0))
    },
    ignoreNULL = FALSE
  )

  observeEvent(event_data("plotly_restyle", source = plot_source), {
    ed <- event_data("plotly_restyle", source = plot_source)
    visible <- ed[[1]][["visible"]]
    if (is.null(visible)) {
      return()
    }
    traces <- unlist(ed[[2]])
    levels_all <- getLevels()
    current <- hiddenGroups()
    for (j in seq_along(traces)) {
      idx <- traces[j] - trace_offset
      if (idx < 0 || idx >= length(levels_all)) next
      level <- levels_all[idx + 1]
      # A restyle event can carry a single visibility value for several trace
      # indices, so fall back to the first when the vector is shorter.
      state <- if (length(visible) >= j) visible[[j]] else visible[[1]]
      current <- if (identical(state, "legendonly")) union(current, level) else setdiff(current, level)
    }
    hiddenGroups(current)
  })

  hiddenGroups
}
