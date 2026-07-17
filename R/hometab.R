#' Build a link that activates a navbar tab client-side
#'
#' Produces an anchor whose click shows the \code{tabPanel} carrying the given
#' \code{value}, using the Bootstrap tab plugin. This keeps landing-page
#' navigation on the client, avoiding cross-namespace \code{updateNavbarPage()}
#' wiring. Tab values targeted this way must be unique across the app.
#'
#' @param label Link label (a string or tag)
#' @param value The \code{value} of the target \code{tabPanel}
#' @param class Optional CSS class for the anchor
#' @param icon Optional icon tag placed before the label
#'
#' @return An anchor tag
#'
#' @keywords internal
#'
navLink <- function(label, value, class = NULL, icon = NULL) {
  tags$a(
    href = "#", class = class,
    onclick = sprintf("var e=document.querySelector('a[data-value=\"%s\"]'); if(e){$(e).tab('show');} return false;", value),
    icon, label
  )
}

#' Shared tab-panel \code{value}s targeted from the landing page
#'
#' Landing-page jump links and tile drawers activate tabs client-side by these
#' \code{value}s, so each must match an explicit \code{value=} on the
#' corresponding \code{tabPanel} in the rnaseq/chipseq/illuminaarray apps.
#'
#' @return A named character vector of tab \code{value}s
#'
#' @keywords internal
#'
homeNavTargets <- function() {
  c(
    samples = "Experiment",
    pca = "pca",
    assay = "assay_tables",
    differential = "diff_tables",
    genesets = "geneset_analyses",
    geneinfo = "geneinfo"
  )
}

#' Build the landing ("Home") tab for a full shinyngs application
#'
#' Produces a full-width Home \code{tabPanel} with a study header, a
#' "Jump to analysis" card, the interactive summary tiles
#' (\code{summarytilesOutput}), the study description, and an unobtrusive
#' footer carrying the interface note and bug-report link. Shared by the
#' rnaseq, chipseq and illuminaarray applications.
#'
#' @param ns Namespace function for the calling application module
#' @param eselist ExploratorySummarizedExperimentList object
#' @param platform Human-readable platform name used in the interface note
#'   (e.g. "RNA-seq")
#'
#' @return A \code{tabPanel}
#'
#' @keywords internal
#'
homeTab <- function(ns, eselist, platform = "RNA-seq") {
  nav <- homeNavTargets()
  jump_items <- list(
    navLink(tagList(icon("bars", verify_fa = FALSE), "Sample metadata"), nav[["samples"]]),
    navLink(tagList(icon("cube", verify_fa = FALSE), "PCA & clustering"), nav[["pca"]])
  )
  if (has_slot_data(eselist, "contrasts")) {
    jump_items <- pushToList(jump_items, navLink(tagList(icon("chart-line", verify_fa = FALSE), "Differential results"), nav[["differential"]]))
  }
  jump_items <- pushToList(jump_items, navLink(tagList(icon("magnifying-glass", verify_fa = FALSE), "Look up a gene"), nav[["geneinfo"]]))

  if (has_slot_data(eselist, "static_pdf")) {
    pdf <- tags$iframe(style = "height:800px; width:100%; scrolling=yes", src = eselist@static_pdf)
  } else {
    pdf <- NULL
  }

  intro_text <- sprintf("This is an interface designed to facilitate downstream %s (and similar) analysis, generated using the Shinyngs package, which makes extensive use of <a href='http://shiny.rstudio.com/'>Shiny</a> and related packages. Most pages have a 'help' link to guide you.", platform)

  sidebar <- bslib::sidebar(
    width = 320,
    open = "desktop",
    div(
      class = "shinyngs-jump",
      h3(class = "shinyngs-eyebrow", "Jump to analysis"),
      jump_items
    ),
    div(
      class = "shinyngs-aside-note",
      p(HTML(intro_text)),
      p(
        HTML(paste0(icon("github"), "&nbsp;Report bugs on the <a href='https://github.com/pinin4fjords/shinyngs'>GitHub page</a>.")),
        br(),
        HTML(paste0(icon("chrome"), "&nbsp;Best viewed in Chrome."))
      )
    )
  )

  mainpanel <- div(
    class = "shinyngs-home",
    h2(class = "shinyngs-study-title", eselist@title),
    h3(class = "shinyngs-study-author", eselist@author),
    div(class = "shinyngs-overview", summarytilesOutput(ns("summarytiles"))),
    div(
      class = "shinyngs-desc",
      h2(class = "shinyngs-eyebrow", "About this study"),
      div(class = "shinyngs-desc-body", HTML(eselist@description)),
      pdf
    )
  )

  bslib::nav_panel(
    "Home",
    bslib::layout_sidebar(sidebar = sidebar, mainpanel, fillable = FALSE),
    icon = icon("house")
  )
}
