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
  shinytest2_require_chrome()

  if (is.null(eselist)) {
    eselist <- shinytest2_eselist()
  }
  app <- prepare_app(type, eselist)
  shinytest2_new_driver(
    shiny::shinyApp(ui = app$ui, server = app$server),
    name = name, timeout = 60000, ...
  )
}

# shinytest2_require_chrome()
#
# chromote allows 10s both for Chrome to open its debugging port and for each
# DevTools command, which a contended CI runner can exceed while launching
# Chrome or navigating to an app. A Chrome that still fails to start skips the
# test, as AppDriver$new() itself does.

shinytest2_require_chrome <- function() {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if(
    is.na(tryCatch(chromote::find_chrome(), error = function(e) NA)),
    "No Chrome/Chromium binary found for headless testing"
  )

  timeout_s <- 60
  options(chromote.timeout = timeout_s)
  chromote_object <- tryCatch(
    chromote::default_chromote_object(),
    error = function(e) testthat::skip(paste("Chrome could not be started:", conditionMessage(e)))
  )
  chromote_object$default_timeout <- timeout_s
}

# shinytest2_new_driver()
#
# If AppDriver's own readiness wait fails, its app and browser are still live,
# so the driver is taken over and repaired rather than rebuilt (a fresh tab can
# hit the same document replacement). Name checks wait until the app has
# settled, since AppDriver runs them before dynamically rendered UI exists.

shinytest2_new_driver <- function(app_dir, name, timeout, ...) {
  driver <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir,
      name = name,
      height = 900, width = 1400, seed = 42, load_timeout = timeout,
      wait = FALSE, check_names = FALSE,
      ...
    ),
    error = function(e) {
      if (!inherits(e$app, "AppDriver") || !grepl("did not become stable", conditionMessage(e), fixed = TRUE)) {
        stop(e)
      }
      e$app$.__enclos_env__$private$state <- "running"
      e$app
    }
  )
  tryCatch(shinytest2_wait_for_app(driver, timeout = timeout), error = function(e) {
    driver$stop()
    stop(e)
  })
}

# shinytest2_wait_for_app()
#
# AppDriver$new() injects its tracer into whichever document is current when
# Page.navigate returns. On a loaded CI runner that document can be replaced,
# and shinytest2 (<= 0.5.1) swallows the resulting destroyed-context error so
# its waits report success, leaving the tracer missing and the dead document's
# session URL cached (rstudio/shinytest2#294). This re-injects the tracer and
# re-reads the session state from the live document, repeating until the
# session it settled on is still live. A late-injected tracer only learns
# busy/idle from later events, so its busy flag is seeded from the class Shiny
# sets while busy.

shinytest2_wait_for_app <- function(app, timeout) {
  deadline <- Sys.time() + timeout / 1000
  repeat {
    settled <- tryCatch(
      shinytest2_attach_live_document(app, deadline),
      error = function(e) {
        if (Sys.time() > deadline) stop(e)
        FALSE
      }
    )
    if (isTRUE(settled)) break
    if (Sys.time() > deadline) {
      stop("Shiny app did not settle on a live session within ", timeout, "ms")
    }
    Sys.sleep(0.2)
  }
  app$expect_unique_names()
  app
}

shinytest2_attach_live_document <- function(app, deadline) {
  session <- app$get_chromote_session()
  remaining_s <- function() max(1, as.numeric(deadline - Sys.time(), units = "secs"))
  run <- function(js) session$Runtime$evaluate(js, returnByValue = TRUE, timeout_ = remaining_s())
  evaluate <- function(js) tryCatch(run(js)$result$value, error = function(e) NULL)

  session_id_js <- "window.Shiny && Shiny.shinyapp && Shiny.shinyapp.config && Shiny.shinyapp.config.sessionId"
  tracer_ready_js <- "typeof window.shinytest2 === 'object' && window.shinytest2.ready === true"

  while (!isTRUE(evaluate(
    "document.readyState === 'complete' && !!window.Shiny && !!Shiny.shinyapp && Shiny.shinyapp.isConnected()"
  ))) {
    if (Sys.time() > deadline) stop("Shiny app did not load and connect in time")
    Sys.sleep(0.1)
  }
  session_id <- evaluate(session_id_js)

  if (!isTRUE(evaluate("typeof window.shinytest2 === 'object'"))) {
    tracer <- system.file("internal", "js", "shiny-tracer.js", package = "shinytest2")
    run(paste(readLines(tracer), collapse = "\n"))
    run("window.shinytest2.busy = document.documentElement.classList.contains('shiny-busy');")
  }

  app$wait_for_js(tracer_ready_js, timeout = remaining_s() * 1000)

  private <- app$.__enclos_env__$private
  private$shiny_test_url <- evaluate("Shiny.shinyapp.getTestSnapshotBaseUrl({fullUrl: true})")
  worker_id <- evaluate("Shiny.shinyapp.config.workerId")
  private$shiny_worker_id <- if (is.null(worker_id) || identical(worker_id, "")) NA_character_ else worker_id

  app$wait_for_idle(duration = 200, timeout = remaining_s() * 1000)

  is.character(session_id) &&
    is.character(private$shiny_test_url) &&
    isTRUE(evaluate(tracer_ready_js)) &&
    identical(evaluate(session_id_js), session_id)
}

# shinytest2_wait_for_outputs()
#
# Shiny can report idle in the gap between the client binding a module's
# dynamically rendered inputs and the server rendering the outputs gated on
# them, so an idle wait alone does not guarantee those outputs exist.

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
  testthat::skip_if_not_installed("shinyngs")
  shinytest2_require_chrome()

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

  shinytest2_new_driver(dir, name = name, timeout = 90000, ...)
}
