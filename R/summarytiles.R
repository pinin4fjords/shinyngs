#' The UI input function of the summarytiles module
#'
#' This module produces a row of summary "tiles" giving an at-a-glance
#' overview of the study: sample count, feature counts, assay data types,
#' contrasts and sample groupings. Clicking a tile opens a detail drawer with
#' a breakdown of that quantity. It is intended for the landing page of an
#' application, but can also be run standalone via \code{prepareApp()}.
#'
#' This input function supplies only the sidebar description used when the
#' module is run standalone; in a full application the tiles are placed in a
#' main panel via \code{summarytilesOutput()}.
#'
#' @param id Submodule namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' summarytilesInput("summary", eselist)
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("summarytiles", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
summarytilesInput <- function(id, eselist) {
  ns <- NS(id)
  p("A high-level summary of the data available in this study. Click any tile for a breakdown.")
}

#' The output function of the summarytiles module
#'
#' This module produces a row of summary "tiles" giving an at-a-glance
#' overview of the study, with a detail drawer that opens beneath them. Both
#' are rendered server-side from the supplied
#' \code{ExploratorySummarizedExperimentList}, so this function just provides
#' the placeholder into which they are drawn.
#'
#' @param id Module namespace
#'
#' @return output An HTML tag object that can be rendered as HTML using
#'   as.character()
#'
#' @keywords shiny
#'
#' @examples
#' summarytilesOutput("summary")
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("summarytiles", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
summarytilesOutput <- function(id) {
  ns <- NS(id)
  uiOutput(ns("tiles"))
}

#' The server function of the summarytiles module
#'
#' This function is called directly, using the same id as its UI counterpart,
#' and wraps its logic in \code{moduleServer()} (see example). It computes a
#' set of summary quantities from the supplied
#' \code{ExploratorySummarizedExperimentList}, renders them as a row of
#' clickable tiles, and shows a detail drawer for the selected tile. Tiles for
#' optional content (contrasts, gene sets) are only shown when that content is
#' present.
#'
#' @param id Module namespace
#' @param eselist ExploratorySummarizedExperimentList object containing
#'   ExploratorySummarizedExperiment objects
#'
#' @keywords shiny
#'
#' @examples
#' summarytiles("summary", eselist)
#'
#' # Almost certainly used via application creation
#'
#' data(zhangneurons)
#' app <- prepareApp("summarytiles", zhangneurons)
#' shiny::shinyApp(ui = app$ui, server = app$server)
#'
summarytiles <- function(id, eselist) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    specs <- summaryTileSpecs(eselist)
    keys <- vapply(specs, function(s) s$key, character(1))
    names(specs) <- keys
    active <- reactiveVal(NULL)

    output$tiles <- renderUI({
      groups <- list(data = "The data", analysis = "The analysis")
      clusters <- lapply(names(groups), function(g) {
        gspecs <- Filter(function(s) s$group == g, specs)
        if (length(gspecs) == 0) {
          return(NULL)
        }
        div(
          class = paste0("cluster cluster-", g),
          p(class = "shinyngs-eyebrow", groups[[g]]),
          div(class = "shinyngs-tiles", lapply(gspecs, function(s) summaryTileTag(ns, s)))
        )
      })
      tagList(
        div(class = "shinyngs-clusters", clusters),
        uiOutput(ns("drawer"))
      )
    })

    for (k in keys) {
      local({
        key <- k
        observeEvent(input[[tileId(key)]],
          {
            active(if (identical(active(), key)) NULL else key)
          },
          ignoreInit = TRUE
        )
      })
    }

    observeEvent(active(),
      {
        for (k in keys) shinyjs::removeClass(id = tileId(k), class = "active")
        act <- active()
        if (!is.null(act)) shinyjs::addClass(id = tileId(act), class = "active")
      },
      ignoreNULL = FALSE
    )

    output$drawer <- renderUI({
      act <- active()
      if (is.null(act)) {
        return(NULL)
      }
      d <- specs[[act]]$detail()
      div(
        class = "shinyngs-drawer",
        div(
          class = "shinyngs-drawer-in",
          div(
            class = "shinyngs-drawer-head",
            h4(class = "shinyngs-drawer-title", HTML(d$title)),
            navLink(d$goto_label, d$goto_value, class = "shinyngs-goto")
          ),
          d$body
        )
      )
    })
  })
}

#' @noRd
tileId <- function(key) paste0("tile_", key)

#' @noRd
fmtInt <- function(x) format(x, big.mark = ",", trim = TRUE)

#' @noRd
kv <- function(term, def) list(tags$dt(term), tags$dd(def))

#' Assemble the summary tile specifications for an experiment list
#'
#' Internal helper returning a list of tile specs. Each spec carries its
#' display fields (key, value, label, icon, group) and a \code{detail}
#' function that builds its drawer content on demand, so tile identity and its
#' detail view stay defined in one place. Optional quantities are only
#' included when present.
#'
#' @param eselist ExploratorySummarizedExperimentList object
#'
#' @return A list of tile-spec lists
#'
#' @keywords internal
#'
summaryTileSpecs <- function(eselist) {
  ese <- eselist[[1]]
  specs <- list()

  specs <- pushToList(specs, list(
    key = "samples", value = ncol(ese), label = "Samples", icon = "users", group = "data",
    detail = function() detailSamples(eselist)
  ))

  for (esen in names(eselist)) {
    specs <- pushToList(specs, local({
      en <- esen
      label <- if (length(eselist) > 1) paste(prettifyVariablename(en), "features") else "Features"
      list(
        key = paste0("features_", en), value = nrow(eselist[[en]]), label = label, icon = "dna", group = "data",
        detail = function() detailFeatures(eselist, en)
      )
    }))
  }

  specs <- pushToList(specs, list(
    key = "assays", value = length(assayNames(ese)), label = "Assay data types", icon = "layer-group", group = "data",
    detail = function() detailAssays(eselist)
  ))

  if (length(eselist@group_vars) > 0) {
    specs <- pushToList(specs, list(
      key = "groups", value = length(eselist@group_vars), label = "Sample groupings", icon = "object-group", group = "analysis",
      detail = function() detailGroups(eselist)
    ))
  }

  if (length(eselist@contrasts) > 0) {
    specs <- pushToList(specs, list(
      key = "contrasts", value = length(eselist@contrasts), label = "Contrasts", icon = "code-compare", group = "analysis",
      detail = function() detailContrasts(eselist)
    ))
  }

  n_gs <- sum(vapply(eselist, function(e) length(e@gene_set_analyses), numeric(1)))
  if (n_gs > 0) {
    specs <- pushToList(specs, list(
      key = "genesets", value = n_gs, label = "Gene set analyses", icon = "list-check", group = "analysis",
      detail = function() detailGenesets(eselist)
    ))
  }

  specs
}

#' Build a single clickable summary tile
#'
#' @param ns Namespace function for the calling module
#' @param spec A tile spec list (see \code{summaryTileSpecs})
#'
#' @return A tag representing the tile, wired as a Shiny action input
#'
#' @keywords internal
#'
summaryTileTag <- function(ns, spec) {
  value <- spec$value
  if (is.numeric(value)) {
    value <- fmtInt(value)
  }
  tags$a(
    id = ns(tileId(spec$key)), href = "#", class = "shinyngs-tile action-button",
    span(class = "shinyngs-tile-icon", icon(spec$icon, verify_fa = FALSE)),
    span(
      class = "shinyngs-tile-body",
      span(class = "shinyngs-tile-value", value),
      span(class = "shinyngs-tile-label", spec$label)
    ),
    span(class = "shinyngs-tile-chev", icon("chevron-down", verify_fa = FALSE))
  )
}

#' Detail-drawer builders, one per tile
#'
#' Each returns a list with \code{title}, \code{goto_label}, \code{goto_value}
#' and a \code{body} tag, consumed by the module's drawer output.
#'
#' @param eselist ExploratorySummarizedExperimentList object
#' @param esen Experiment name (features builder only)
#'
#' @return A drawer-content list
#'
#' @keywords internal
#'
detailSamples <- function(eselist) {
  ese <- eselist[[1]]
  gv <- defaultGroupvar(eselist)
  if (is.null(gv)) {
    return(list(
      title = "Samples", goto_label = "Open sample metadata →", goto_value = homeNavTargets()[["samples"]],
      body = p(class = "shinyngs-muted", paste(ncol(ese), "samples, with no grouping variable defined."))
    ))
  }
  tab <- sort(table(as.character(colData(ese)[[gv]])), decreasing = TRUE)
  maxv <- max(tab)
  rows <- lapply(names(tab), function(nm) {
    div(
      class = "shinyngs-bar-row",
      span(class = "shinyngs-bar-name", title = nm, nm),
      span(class = "shinyngs-bar-track", span(class = "shinyngs-bar-fill", style = paste0("width:", round(tab[[nm]] / maxv * 100), "%"))),
      span(class = "shinyngs-bar-val", tab[[nm]])
    )
  })
  list(
    title = paste0("Samples <span>· ", ncol(ese), " libraries across ", length(tab), " ", prettifyVariablename(gv), " levels</span>"),
    goto_label = "Open sample metadata →", goto_value = homeNavTargets()[["samples"]],
    body = div(class = "shinyngs-bars", rows)
  )
}

#' @rdname detailSamples
detailFeatures <- function(eselist, esen) {
  e <- eselist[[esen]]
  items <- list(kv("Quantified in", paste(length(assayNames(e)), "assays:", paste(prettifyVariablename(assayNames(e)), collapse = ", "))))
  lf <- e@labelfield
  if (length(lf) > 0 && lf %in% colnames(mcols(e))) {
    col <- mcols(e)[[lf]]
    annotated <- sum(!is.na(col) & nzchar(as.character(col)))
    items <- c(items, list(kv(paste0("With ", prettifyVariablename(lf)), paste0(fmtInt(annotated), " of ", fmtInt(nrow(e)), " annotated"))))
  }
  if (length(eselist) > 1) {
    items <- c(items, list(kv("Level", paste0(prettifyVariablename(esen), "-level quantification"))))
  }
  list(
    title = paste0(prettifyVariablename(esen), " features <span>· ", fmtInt(nrow(e)), " quantified</span>"),
    goto_label = "Open assay data tables →", goto_value = homeNavTargets()[["assay"]],
    body = tags$dl(class = "shinyngs-kv", items)
  )
}

#' @rdname detailSamples
detailAssays <- function(eselist) {
  an <- assayNames(eselist[[1]])
  list(
    title = paste0("Assay data types <span>· ", length(an), " matrices</span>"),
    goto_label = "Open assay data tables →", goto_value = homeNavTargets()[["assay"]],
    body = div(class = "shinyngs-pills", lapply(an, function(a) span(class = "shinyngs-pill", prettifyVariablename(a))))
  )
}

#' @rdname detailSamples
detailGroups <- function(eselist) {
  ese <- eselist[[1]]
  default_gv <- defaultGroupvar(eselist)
  items <- lapply(eselist@group_vars, function(gv) {
    nlev <- length(unique(colData(ese)[[gv]]))
    term <- if (!is.null(default_gv) && identical(gv, default_gv)) {
      tagList(prettifyVariablename(gv), span(class = "shinyngs-pill", "default"))
    } else {
      prettifyVariablename(gv)
    }
    kv(term, paste(nlev, "levels"))
  })
  list(
    title = paste0("Sample groupings <span>· ", length(eselist@group_vars), " variables</span>"),
    goto_label = "Open sample metadata →", goto_value = homeNavTargets()[["samples"]],
    body = tags$dl(class = "shinyngs-kv", items)
  )
}

#' @rdname detailSamples
detailContrasts <- function(eselist) {
  contrasts <- eselist@contrasts
  has_stats <- any(vapply(eselist, function(e) length(e@contrast_stats) > 0, logical(1)))
  ncap <- 12
  rows <- lapply(utils::head(contrasts, ncap), function(ct) {
    tags$tr(
      tags$td(prettifyVariablename(ct[1])),
      tags$td(ct[2]),
      tags$td(ct[3]),
      if (has_stats) tags$td(span(class = "shinyngs-avail", "✓ fold-change · p · q")) else NULL
    )
  })
  header <- tags$tr(tags$th("Variable"), tags$th("Reference"), tags$th("Compared"), if (has_stats) tags$th("Stats") else NULL)
  more <- if (length(contrasts) > ncap) p(class = "shinyngs-muted", paste0("+ ", length(contrasts) - ncap, " more")) else NULL
  list(
    title = paste0("Contrasts <span>· ", length(contrasts), " differential comparisons</span>"),
    goto_label = "Open differential results →", goto_value = homeNavTargets()[["differential"]],
    body = tagList(tags$table(class = "shinyngs-ctable", tags$thead(header), tags$tbody(rows)), more)
  )
}

#' @rdname detailSamples
detailGenesets <- function(eselist) {
  multi <- length(eselist) > 1
  items <- list()
  for (esen in names(eselist)) {
    for (a in names(eselist[[esen]]@gene_set_analyses)) {
      term <- if (multi) paste0(prettifyVariablename(esen), " · ", prettifyVariablename(a)) else prettifyVariablename(a)
      items <- c(items, list(kv(term, "gene set analysis available")))
    }
  }
  list(
    title = "Gene set analyses",
    goto_label = "Open gene set analyses →", goto_value = homeNavTargets()[["genesets"]],
    body = tags$dl(class = "shinyngs-kv", items)
  )
}
