# shinytest2_eselist()
#
# A small synthetic ExploratorySummarizedExperimentList with contrasts and
# contrast_stats populated, so PCA/heatmap/differential-table paths all have
# something to render. Built once per test session and cached.

shinytest2_eselist <- local({
  eselist <- NULL
  function() {
    if (is.null(eselist)) {
      set.seed(42)

      n_genes <- 60
      n_samples <- 12

      counts <- matrix(
        stats::rnbinom(n_genes * n_samples, mu = 200, size = 5),
        nrow = n_genes, ncol = n_samples
      )
      rownames(counts) <- paste0("gene", seq_len(n_genes))
      colnames(counts) <- paste0("sample", seq_len(n_samples))

      coldata <- S4Vectors::DataFrame(
        row.names = colnames(counts),
        condition = rep(c("control", "treated"), each = n_samples / 2),
        batch = rep(c("batch1", "batch2"), times = n_samples / 2)
      )

      annotation <- data.frame(
        gene_id = rownames(counts),
        gene_name = paste0("Gene", seq_len(n_genes)),
        row.names = rownames(counts)
      )

      pvals <- matrix(stats::runif(n_genes), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
      qvals <- matrix(stats::p.adjust(pvals[, 1], method = "BH"), nrow = n_genes, dimnames = list(rownames(counts), "V1"))
      fold_changes <- matrix(stats::rnorm(n_genes, sd = 2), nrow = n_genes, dimnames = list(rownames(counts), "V1"))

      ese <- ExploratorySummarizedExperiment(
        assays = S4Vectors::SimpleList(counts = counts),
        colData = coldata,
        annotation = annotation,
        idfield = "gene_id",
        labelfield = "gene_name",
        contrast_stats = list(counts = list(pvals = pvals, qvals = qvals, fold_changes = fold_changes))
      )

      new_eselist <- ExploratorySummarizedExperimentList(
        eses = list(counts = ese),
        title = "shinytest2 smoke test study",
        author = "shinytest2",
        description = "Synthetic dataset for shinytest2 smoke tests",
        group_vars = c("condition", "batch"),
        default_groupvar = "condition"
      )
      new_eselist@contrasts <- list(
        list(id = "condition_control_treated", Variable = "condition", Group.1 = "control", Group.2 = "treated")
      )

      eselist <<- new_eselist
    }
    eselist
  }
})

# shinytest2_app_driver()
#
# Launch a shinytest2::AppDriver for a prepare_app() type, skipping the test if
# no headless Chrome is available.

shinytest2_app_driver <- function(type, name, eselist = NULL, ...) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  shinytest2_start_chromote()

  if (is.null(eselist)) {
    eselist <- shinytest2_eselist()
  }
  app <- prepare_app(type, eselist)
  driver <- shinytest2::AppDriver$new(
    shiny::shinyApp(ui = app$ui, server = app$server),
    name = name,
    height = 900, width = 1400, seed = 42, load_timeout = 60000,
    wait = FALSE,
    ...
  )
  shinytest2_wait_for_app(driver, timeout = 60000)
}

# shinytest2_start_chromote()
#
# chromote allows 10s both for Chrome to open its debugging port and for each
# DevTools command, which a contended CI runner can exceed while launching
# Chrome or navigating to an app. A Chrome that still fails to start skips the
# test, as AppDriver$new() itself does.

shinytest2_start_chromote <- function(timeout = 60) {
  options(chromote.timeout = timeout)
  chromote_object <- tryCatch(
    chromote::default_chromote_object(),
    error = function(e) testthat::skip(paste("Chrome could not be started:", conditionMessage(e)))
  )
  chromote_object$default_timeout <- timeout
  invisible(chromote_object)
}

# shinytest2_wait_for_app()
#
# AppDriver$new() injects its tracer into whichever document is current when
# Page.navigate returns. On a loaded CI runner that document can be replaced
# before the app has loaded; shinytest2 (<= 0.5.1) swallows the resulting
# destroyed-context error from its readiness wait, then caches that document's
# session URL, so later idle waits hit the missing tracer and value reads hit
# a closed session (rstudio/shinytest2#294). Drivers are therefore created with
# wait = FALSE and readiness is confirmed against the live document,
# re-injecting the tracer if it was lost and re-reading the session state.

shinytest2_wait_for_app <- function(app, timeout) {
  session <- app$get_chromote_session()
  evaluate <- function(js) {
    tryCatch(
      session$Runtime$evaluate(js, returnByValue = TRUE)$result$value,
      error = function(e) NULL
    )
  }

  deadline <- Sys.time() + timeout / 1000
  while (!isTRUE(evaluate(
    "document.readyState === 'complete' && !!window.Shiny && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()"
  ))) {
    if (Sys.time() > deadline) {
      stop("Shiny app did not load and connect within ", timeout, "ms")
    }
    Sys.sleep(0.1)
  }

  if (!isTRUE(evaluate("typeof window.shinytest2 === 'object'"))) {
    tracer <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
    session$Runtime$evaluate(paste(readLines(tracer), collapse = "\n"))
  }

  app$wait_for_js("window.shinytest2.ready === true", timeout = timeout)

  private <- app$.__enclos_env__$private
  private$shiny_test_url <- evaluate("Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl: true})")
  worker_id <- evaluate("Shiny.shinyapp.config.workerId")
  private$shiny_worker_id <- if (is.null(worker_id) || identical(worker_id, "")) NA_character_ else worker_id

  app$wait_for_idle(duration = 200, timeout = timeout)
  app
}

# shinytest2_wait_for_outputs()
#
# Shiny can report idle in the gap between the client binding a module's
# dynamically rendered inputs and the server rendering the outputs gated on
# them, so tests wait for the outputs they assert on rather than for idle.

shinytest2_wait_for_outputs <- function(app, outputs, timeout = 20000) {
  for (output in outputs) {
    app$wait_for_value(output = output, timeout = timeout)
  }
  invisible(app)
}

# shinytest2_bookmark_app_driver()
#
# Launch an AppDriver against an on-disk app.R rather than a shinyApp object.
# URL bookmarking must be enabled when the session is constructed (that is when
# module onBookmark hooks register and Shiny decides whether to keep the share
# button), which prepare_app() does. Passing a shinyApp object to AppDriver runs
# it in a fresh process that never called prepare_app, so bookmarking would be
# off; running from an app.R that calls prepare_app in that process mirrors real
# use. Requires shinyngs to be installed (it is under R CMD check).

shinytest2_bookmark_app_driver <- function(type, name, ...) {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("shinyngs")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  shinytest2_start_chromote()

  dir <- withr::local_tempdir(.local_envir = parent.frame())
  saveRDS(shinytest2_eselist(), file.path(dir, "eselist.rds"))
  writeLines(
    c(
      "library(shinyngs)",
      "eselist <- readRDS('eselist.rds')",
      sprintf("app <- prepare_app('%s', eselist)", type),
      "shiny::shinyApp(app$ui, app$server)"
    ),
    file.path(dir, "app.R")
  )

  driver <- shinytest2::AppDriver$new(
    dir,
    name = name,
    height = 900, width = 1400, seed = 42, load_timeout = 90000,
    wait = FALSE,
    ...
  )
  shinytest2_wait_for_app(driver, timeout = 90000)
}
